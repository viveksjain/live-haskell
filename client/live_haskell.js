function LiveHaskell() {
  this._editor = createEditor('editor');
  this._editor.setOptions({
    theme: 'ace/theme/monokai',
    mode: 'ace/mode/haskell',
    enableBasicAutocompletion: true,
  });
  this._editCounter = 0;
  var that = this;
  this.onChange(function(ev) {
    that._editCounter++;
  });
  this._editor.commands.addCommand({
    name: 'getType',
    bindKey: {win: 'Ctrl-Alt-;', mac: 'Command-Option-;'},
    exec: function (editor) {
      that.getType();
    }
  });

  this._output = createReadOnlyEditor('output');
}

function createEditor(elem) {
  var editor = ace.edit(elem);
  editor.$blockScrolling = Infinity;  // Hide Ace error message about this
  editor.setFontSize(14);
  editor.session.setTabSize(2);
  return editor;
}

function createReadOnlyEditor(elem) {
  var editor = createEditor(elem);
  editor.setOptions({
    readOnly: true,
    highlightActiveLine: false,
    showGutter: false,
  });
  // Hack - hide cursor
  editor.renderer.$cursorLayer.element.style.opacity = 0;
  return editor;
}

LiveHaskell.prototype.setFilename = function(filename) {
  this._filename = filename;
}

LiveHaskell.prototype.setInput = function(contents) {
  this._editor.setValue(contents, 1);
  this.traceInput();
}

LiveHaskell.prototype.setOutput = function(contents) {
  this._output.setValue(contents, 1);
  this._refresher.setRefreshing(false);
}

// Show and start appropriate listeners.
LiveHaskell.prototype.enable = function(fileSelector) {
  if (!this._isEnabled) {
    this._isEnabled = true;
    fileSelector.hide();
    $('#live_haskell').show();

    if (!localStorage.seen) {
      try {
        // Fails in Safari private browsing mode
        localStorage.seen = true;
      } catch (e) {}
      showHelp();
    }

    var that = this;
    var debounced = debounce(function() {
      that.traceInput();
    }, 300, {
      leading: true,
      trailing: true,
    });
    // Evaluate on enter inside editor. Needs to be keyup so we get updated
    // input text.
    $('#editor').keyup(function(ev) {
      if (ev.which == 13 && !(ev.metaKey || ev.ctrlKey)) {
        // Debounce enter key, but not Cmd-/Ctrl-Enter since the user is
        // explicitly evaluating in the latter case.
        debounced();
      }
    });

    // Evaluate on Cmd-/Ctrl-Enter anywhere in the window.
    $(document).keydown(function(ev) {
      if (ev.which == 13 && (ev.metaKey || ev.ctrlKey)) {
        ev.preventDefault();
        that.traceInput();
      }
    });
  }
}

LiveHaskell.prototype.traceInput = function() {
  var that = this;
  this._reloadInput(function(isReloaded, hasErrors) {
    if (hasErrors) return;
    var command = that._commander.getInput();
    if (!isReloaded && that._evaluatedCommand == command) {
      console.log('Command already evaluated');
      return;
    }
    that._evaluatedCommand = command;
    $.post('trace', {
      filename: that._filename,
      script: that._evaluatedCommand,
    }, function(result) {
      console.log(result);
    });
  });
}

// `cb` is passed whether the input was reloaded, and whether the input has
// errors.
LiveHaskell.prototype._reloadInput = function(cb) {
  if (!this.isOutputChanged()) {
    console.log('Output already up-to-date');
    if (cb) cb(false, this._hadErrors);
    return;
  }
  var editCount = this._editCounter;
  this._refresher.setRefreshing(true);
  var that = this;
  $.post('reload', {
    filename: this._filename,
    script: this._editor.getValue(),
  }, function(result) {
    console.log(result);
    that._evaluatedEditCount = editCount;
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
    that._hadErrors = aceErrors.length != 0;
    if (cb) cb(true, that._hadErrors);
  });
}

LiveHaskell.prototype.getType = function () {
  var that = this;
  this._reloadInput(function() {
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
      filename: that._filename || '',
      line_start: range.start.row + 1,  // 0 indexed
      col_start: range.start.column,
      line_end: range.end.row + 1,
      col_end: range.end.column,
      text: that._editor.session.getTextRange(range),
    }, function (result) {
      console.log(result);
      // TODO check for errs
      var $elem;
      if (hasSelection) {
        $elem = $('#editor .ace_selection');
      } else {
        $elem = $('#editor .ace_cursor');
      }
      new Tooltip($elem, result.output, that._editor);
    });
  });
}

LiveHaskell.prototype.setRefresher = function(refresher) {
  this._refresher = refresher;
}

LiveHaskell.prototype.setCommander = function(commander) {
  this._commander = commander;
}

LiveHaskell.prototype.onChange = function(cb) {
  this._editor.session.on('change', cb);
}

// Return true if the current buffer is not the same as what was most recently
// evaluated.
LiveHaskell.prototype.isOutputChanged = function() {
  return this._editCounter !== this._evaluatedEditCount;
}
