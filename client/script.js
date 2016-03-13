function show_help() {
  $('body').chardinJs('start');
}

function FileSelector(live_haskell) {
  var that = this;
  if (localStorage.filename) {
    $('#form_filename').val(localStorage.filename);
  }
  $('#file_selector').submit(function(ev) {
    ev.preventDefault();

    var filename = $('#form_filename').val();
    // TODO
    // $.post('open', {filename: filename}, function (result) {
    // });

    // On success
    localStorage.filename = filename;
    live_haskell.setFilename(filename);
    var contents = "-- Type here and it will get evaluated when you press enter (careful, make sure\n-- you don't execute any potentially dangerous code!)\nmain' :: String\nmain' = \"Hello world\"\ntest = map (+)";
    live_haskell.setInput(contents);
    live_haskell.enable(that);
  });
}

FileSelector.prototype.hide = function() {
  $('#file_selector_container').hide();
}

function Refresher(live_haskell) {
  live_haskell.setRefresher(this);
  this._$elem = $('#output_overlay');
  this._$icon = $('#refresh_icon');
  this._live_haskell = live_haskell;

  var that = this;
  live_haskell.onChange(function(ev) {
    that._setRefreshable(true);
  });
  this._$icon.click(function(ev) {
    live_haskell.evaluateInput();
  })
}

Refresher.prototype._setRefreshable = function(isRefreshable) {
  if (isRefreshable) {
    this._$elem.addClass('refreshable');
  } else {
    this._$elem.removeClass('refreshable');
  }
}

Refresher.prototype.setRefreshing = function(isRefreshing) {
  if (isRefreshing) {
    this._$icon.addClass('rotating');
  } else {
    this._$icon.removeClass('rotating');
    if (!this._live_haskell.isOutputChanged()) {
      this._setRefreshable(false);
    }
  }
};

function Commander(live_haskell) {
  this._ace = createEditor('command');
  // Based on http://stackoverflow.com/a/32316070
  this._ace.setOptions({
    maxLines: 1,
    highlightActiveLine: false,
    showGutter: false,
    mode: 'ace/mode/haskell',
    theme: 'ace/theme/xcode',
    printMargin: false,
  });
  this._ace.on('paste', function(ev) {
    ev.text = ev.text.replace(/[\r\n]+/g, " ");
  });
  this._ace.renderer.screenToTextCoordinates = function(x, y) {
    var pos = this.pixelToScreenCoordinates(x, y);
    return this.session.screenToDocumentPosition(
      Math.min(this.session.getScreenLength() - 1, Math.max(pos.row, 0)),
      Math.max(pos.column, 0)
    );
  };

  var that = this;
  this._ace.commands.bindKey('Enter|Shift-Enter|Alt-Enter', function(editor) {
    live_haskell.trace(editor.getValue());
  });
  this._ace.setValue('main', 1);
}
