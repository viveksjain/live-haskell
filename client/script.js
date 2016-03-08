$(document).ready(function () {
  // Purposely a global so it can be used anywhere, and to allow for
  // easier debugging.
  live_haskell = new LiveHaskell();
  new FileSelector(live_haskell);
});

function FileSelector(live_haskell) {
  var that = this;
  $('#file_selector').submit(function(ev) {
    ev.preventDefault();

    // TODO
    // $.post('open', {filename: $('#form_filename').val()}, function (result) {
    // });
    var contents = "-- Type here and it will get evaluated when you press enter (careful, make sure\n-- you don't execute any potentially dangerous code!)\nmain' :: String\nmain' = \"Hello world\"\ntest = map (+)";
    live_haskell.setInput(contents);
    live_haskell.enable(that);
  });
}

FileSelector.prototype.hide = function() {
  $('#file_selector_container').hide();
}

function LiveHaskell() {
  this._editor = ace.edit('editor');
  this._editor.$blockScrolling = Infinity;  // Hide Ace error message about this
  this._editor.setTheme('ace/theme/monokai');
  this._editor.session.setMode('ace/mode/haskell');
  this._editor.session.setTabSize(2);
  var that = this;
  this._editor.commands.addCommand({
    name: 'getType',
    bindKey: {win: 'Ctrl-Alt-T', mac: 'Command-Option-T'},
    exec: function (editor) {
      that.getType();
    }
  });

  this._output = ace.edit('output');
  this._output.$blockScrolling = Infinity;  // Hide Ace error message about this
  this._output.setOptions({
    readOnly: true,
    highlightActiveLine: false,
    showGutter: false
  });
  // Hack - hide cursor
  this._output.renderer.$cursorLayer.element.style.opacity = 0;
}

LiveHaskell.prototype.setInput = function(contents) {
  this._editor.setValue(contents, 1);
}

LiveHaskell.prototype.setOutput = function(contents) {
  this._output.setValue(contents, 1);
}

LiveHaskell.prototype.enable = function(file_selector) {
  if (!this._isEnabled) {
    this._isEnabled = true;
    file_selector.hide();
    $('#live_haskell').show();
    var that = this;
    // Evaluate on enter inside editor. Needs to be keyup so we get updated
    // input text.
    $('#editor').keyup(function(ev) {
      if (ev.which == 13 && !(ev.metaKey || ev.ctrlKey)) {
        that.evaluateInput();
      }
    });

    // Evaluate on Cmd-/Ctrl-Enter anywhere in the window.
    $(document).keydown(function(ev) {
      if (ev.which == 13 && (ev.metaKey || ev.ctrlKey)) {
        ev.preventDefault();
        that.evaluateInput();
      }
    });
  }
}

LiveHaskell.prototype.evaluateInput = function(cb) {
  var that = this;
  $.post('evaluate', {script: this._editor.getValue()}, function(result) {
    console.log(result);
    that.setOutput(result.output || result.error, 1);

    var errors = result.errors;
    var aceErrors = [];
    for (var k in errors) {
      if (errors.hasOwnProperty(k)) {
        aceErrors.push({
          row: k - 1,  // 0 indexed
          text: errors[k],
          type: 'error',
        });
      }
    }
    that._editor.session.setAnnotations(aceErrors);
    if (cb) cb();
  });
}

LiveHaskell.prototype.getType = function () {
  var that = this;
  this.evaluateInput(function() {
    var range;
    var hasSelection;
    if (that._editor.getSelection().isEmpty()) {
      range = that._editor.getSelection().getWordRange();
      hasSelection = false;
    } else {
      range = that._editor.getSelectionRange();
      hasSelection = true;
    }
    $.post('type-at', {
      line_start: range.start.row + 1,  // 0 indexed
      col_start: range.start.column,
      line_end: range.end.row + 1,
      col_end: range.end.column,
      text: that._editor.session.getTextRange(range),
    }, function (result) {
      // TODO check for errs
      new Tooltip(result.output);
    });
  });
}
