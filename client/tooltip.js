var _tooltip = null;
// Number of pixels away from the tooltip that the mouse will have to move to
// close the tooltip.
var TOOLTIP_DISTANCE = 100;

function Tooltip($elem, text, lhEditor) {
  if (_tooltip != null) {
    _tooltip.destroy();
  }

  this._listeners = {};
  // Remove leading and trailing whitespace
  text = text.replace(/^\s*|\s*$/g, '');
  // Make sure we exclude tooltips (since they contain ace as well) from
  // selection.
  var $elem = $elem.filter(function() {
    return $(this).closest('.type_tooltip').length == 0;
  });
  this._lhEditor = lhEditor;
  var that = _tooltip = this;
  this._tooltip = $elem.qtip({
    content: {text: '<div class="type_tooltip_ace"></div>'},
    show: {ready: true,},
    hide: {
      leave: (function() {if (DEBUG) return false; else return "window";})(),
    },
    events: {
      hidden: function(ev, api) {
        that.destroy();
      },
      render: function(ev, api) {
        // Limit tooltip to its containing editor's size
        that._tooltip.tooltip.css('max-width', $('#editor .ace_scroller').width());
        that._createAce(text);
        that._startListeners();
      },
    },
    position: {
      my: 'top center',
      at: 'bottom center',
      adjust: {method: 'shift',},
      container: $('.ace_content'),
      viewport: true,
      effect: false,
    },
    style: {
      classes: 'qtip-tipsy type_tooltip',
    },
  }).qtip('api');
}

Tooltip.prototype.destroy = function() {
  if (this._isDestroyed) return;
  this._isDestroyed = true;
  this._stopListeners();
  this._ace.destroy();
  this._tooltip.destroy();
  _tooltip = null;
}

Tooltip.prototype._createAce = function(text) {
  this._ace = createReadOnlyEditor(this._tooltip.tooltip.find('.type_tooltip_ace').get(0));
  this._ace.setValue(text, 1);
  this._ace.setOptions({maxLines: Infinity,});
  this._ace.setTheme('ace/theme/monokai');
  this._ace.session.setMode('ace/mode/haskell');
  var that = this;
  this._ace.renderer.once('afterRender', function() {
    // Set to ace width, leaving space for padding
    var width = $('.type_tooltip_ace .ace_text-layer').width() + 30;
    var tooltip = that._tooltip.tooltip;
    tooltip.css('width', width);
    that._tooltip.reposition(null, false);
    // Remove ace scrolling
    that._ace.resize();

    if (that._isOutsideWindow(true)) {
      that._tooltip.set('position.my', 'bottom center');
      that._tooltip.set('position.at', 'top center');
      that._tooltip.reposition(null, false);
    }

    var renderListener = that._genRenderListener();
    that._listeners.render = renderListener;
    that._lhEditor.renderer.on('afterRender', renderListener);
  });
}

Tooltip.prototype._isOutsideWindow = function(checkBelowOnly) {
  var tooltip = this._tooltip.tooltip;
  var offset = tooltip.offset();
  return (!checkBelowOnly && offset.top < 0) ||
    offset.top + tooltip.height() > $(window).height();
}

// Somewhat hacky, but due to Ace's rendering we need to handle scrolling
// ourselves whenever the first line number changes.
Tooltip.prototype._genRenderListener = function() {
    var renderer = this._lhEditor.renderer;
    var firstRowScreen = renderer.layerConfig.firstRowScreen;
    var top = parseFloat(this._tooltip.tooltip.css('top'));
    var that = this;
    return function(ev) {
      if (renderer.layerConfig.firstRowScreen != firstRowScreen) {
        var delta = (firstRowScreen - renderer.layerConfig.firstRowScreen) * renderer.layerConfig.lineHeight;
        top += delta;
        that._tooltip.tooltip.css('top', top);
        if (that._isOutsideWindow()) {
          that.destroy();
        }
        firstRowScreen = renderer.layerConfig.firstRowScreen;
      }
    };
}

// Add a listener to hide the tooltip if the mouse moves more than
// `TOOLTIP_DISTANCE` pixels from the tooltip, or a key is pressed and the mouse
// is not inside the tooltip (this still allows copying the tooltip).
Tooltip.prototype._startListeners = function() {
  var mouseListener = this._genMouseListener();
  var keyListener = this._genKeyListener();
  var that = this;
  // Save so we can call off using them
  this._listeners.mouse = mouseListener;
  this._listeners.key = keyListener;
  if (!DEBUG) {
    $(document).mousemove(mouseListener);
    $(document).keydown(keyListener);
  }
  // This is required to allow the tooltip Ace to actually be focused -
  // otherwise, the mousedown event is triggered in tooltip Ace, then in editor
  // Ace, but this means that the enclosing editor Ace gets the last event and
  // will always be focused. We simply stop propagation in the tooltip Ace so
  // that we end up with the focus.
  this._ace.on('mousedown', function(ev) {ev.stopPropagation();});
}

Tooltip.prototype._stopListeners = function() {
  if (this._listeners != null) {
    $(document).off('mousemove', this._listeners.mouse);
    $(document).off('keydown', this._listeners.key);
    this._lhEditor.renderer.off('afterRender', this._listeners.render);
    this._listeners = null;
  }
}

Tooltip.prototype._genMouseListener = function() {
  var that = this;
  return function(ev) {
    that._mouse = {x: ev.pageX, y: ev.pageY};
    if (that._getDistance() > TOOLTIP_DISTANCE &&
        that._tooltip.tooltip.find('.ace_selecting').length == 0) {
      that.destroy();
    }
  }
}

Tooltip.prototype._getDistance = function() {
  var offset = this._tooltip.tooltip.offset();
  var width = this._tooltip.tooltip.width();
  var height = this._tooltip.tooltip.height();
  var x = this._getDimensionDistance(this._mouse.x, offset.left, width);
  var y = this._getDimensionDistance(this._mouse.y, offset.top, height);
  return Math.floor(Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2)));
}

Tooltip.prototype._getDimensionDistance = function(mouse, start, len) {
  if (mouse < start) return start - mouse;
  else if (mouse < start + len) return 0;
  else return mouse - start - len;
}

Tooltip.prototype._genKeyListener = function() {
  var that = this;
  return function(ev) {
    if (ev.metaKey || ev.ctrlKey) return;
    // C, to allow for copying without closing tooltip
    if (ev.keyCode == 67) return;
    that.destroy();
  }
}
