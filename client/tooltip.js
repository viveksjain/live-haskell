var _tooltip = null;
// Number of pixels away from the tooltip that the mouse will have to move to
// close the tooltip.
var DISTANCE = 100;

function Tooltip(text) {
  if (_tooltip != null) {
    _tooltip.destroy();
  }

  _tooltip = this;
  var that = this;
  this._tooltip = $('.ace_selection').qtip({
    content: {text: text,},
    show: {ready: true,},
    events: {
      hidden: function(ev, api) {
        that.destroy();
      },
      render: function(ev, api) {
        that._startListeners();
      },
    },
    position: {
      my: 'top center',
      at: 'bottom center',
      viewport: $('.ace_content'),
      adjust: {method: 'shift',},
      container: $('.ace_content'),
    },
    style: {
      classes: 'qtip-tipsy',
    },
  }).qtip('api');
}

Tooltip.prototype.destroy = function() {
  this._tooltip.destroy();
  this._stopListeners();
  _tooltip = null;
}

// Add a listener to hide the tooltip if the mouse moves more than `DISTANCE`
// pixels from the tooltip, or a key is pressed and the mouse is not inside the
// tooltip (this still allows copying the tooltip).
Tooltip.prototype._startListeners = function() {
  var mouseListener = this._genMouseListener();
  var keyListener = this._genKeyListener();
  // Save so we can call off using them
  this._listeners = {
    mouse: mouseListener,
    key: keyListener,
  };
  $(document).mousemove(mouseListener);
  $(document).keydown(keyListener);
  // Override only to stop propagation and thus stop the .ace_scroller listener
  // from receiving the event, thus allowing the default action to occur.
  this._tooltip.tooltip.mousedown(function(ev) {ev.stopPropagation();});
}

Tooltip.prototype._stopListeners = function() {
  if (this._listeners != null) {
    $(document).off('mousemove', this._listeners.mouse);
    $(document).off('keydown', this._listeners.key);
    this._listeners = null;
  }
}

Tooltip.prototype._genMouseListener = function() {
  var that = this;
  return function(ev) {
    that._mouse = {x: ev.pageX, y: ev.pageY};
    if (that._getDistance() > DISTANCE) {
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
