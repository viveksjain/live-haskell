function showHelp() {
  $('body').chardinJs('start');
}

function FileSelector(liveHaskell) {
  if (localStorage.filename) {
    $('#form_filename').val(localStorage.filename);
  }
  var that = this;
  $('#file_selector').submit(function(ev) {
    ev.preventDefault();

    var filename = $('#form_filename').val();
    $.post('open', {filename: filename}, function (result) {
      // TODO handle errors
      console.log(result);
      // On success
      try {
        // Fails in Safari private browsing mode
        localStorage.filename = filename;
      } catch(e) {}
      liveHaskell.setFilename(filename);
      liveHaskell.setInput(result.details);
      liveHaskell.enable(that);
    });

  });
}

FileSelector.prototype.hide = function() {
  $('#file_selector_container').hide();
}

function Refresher(liveHaskell) {
  liveHaskell.setRefresher(this);
  this._$elem = $('#output_overlay');
  this._$icon = $('#refresh_icon');
  this._liveHaskell = liveHaskell;

  var that = this;
  liveHaskell.onChange(function(ev) {
    that._setRefreshable(true);
  });
  this._$icon.click(function(ev) {
    liveHaskell.traceInput();
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
    if (!this._liveHaskell.isOutputChanged()) {
      this._setRefreshable(false);
    }
  }
};

function Commander(liveHaskell) {
  this._ace = createEditor('command');
  liveHaskell.setCommander(this);
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
    liveHaskell.traceInput();
  });
  this._ace.setValue('main', 1);
}

Commander.prototype.getInput = function() {
  return this._ace.getValue();
};
