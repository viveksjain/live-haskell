var _tooltip = null;
// Number of pixels away from the tooltip that the mouse will have to move to
// close the tooltip.
var TOOLTIP_DISTANCE = 100;

function Tooltip($elem, text, lh_editor) {
  if (_tooltip != null) {
    _tooltip.destroy();
  }

  // Remove leading and trailing whitespace
  text = text.replace(/^\s*/, '').replace(/\s*$/, '');
  // Make sure we exclude tooltips (since they contain ace as well) from
  // selection.
  var $elem = $elem.filter(function() {
    return $(this).closest('.type_tooltip').length == 0;
  });
  this._lh_editor = lh_editor;
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
      },
      show: function(ev, api) {
        that._startListeners();
      },
    },
    position: {
      my: 'top center',
      at: 'bottom center',
      viewport: $('#editor'),
      adjust: {method: 'shift',},
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
    that._tooltip.tooltip.css('width', width);
    that._tooltip.reposition(null, false);
    // Remove ace scrolling
    that._ace.resize();
  });
}

// Add a listener to hide the tooltip if the mouse moves more than
// `TOOLTIP_DISTANCE` pixels from the tooltip, or a key is pressed and the mouse
// is not inside the tooltip (this still allows copying the tooltip). Also
// listen for scrolling events from the editor
Tooltip.prototype._startListeners = function() {
  var mouseListener = this._genMouseListener();
  var keyListener = this._genKeyListener();
  var that = this;
  var scrollListener = function() {
    console.log('here!!!');
    that.destroy();
  }
  // Save so we can call off using them
  this._listeners = {
    mouse: mouseListener,
    key: keyListener,
    scroll: scrollListener,
  };
  if (!DEBUG) {
    $(document).mousemove(mouseListener);
    $(document).keydown(keyListener);
  }

  this._lh_editor.session.on('changeScrollLeft', scrollListener);
  this._lh_editor.session.on('changeScrollTop', scrollListener);
}

Tooltip.prototype._stopListeners = function() {
  if (this._listeners != null) {
    $(document).off('mousemove', this._listeners.mouse);
    $(document).off('keydown', this._listeners.key);
    this._lh_editor.session.off('changeScrollLeft', this._listeners.scroll);
    this._lh_editor.session.off('changeScrollTop', this._listeners.scroll);
    this._listeners = null;
  }
}

Tooltip.prototype._genMouseListener = function() {
  var that = this;
  return function(ev) {
    that._mouse = {x: ev.pageX, y: ev.pageY};
    if (that._getDistance() > TOOLTIP_DISTANCE) {
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
    if (!that._mouse || that._getDistance() > 0) {
      that.destroy();
    }
  }
}
