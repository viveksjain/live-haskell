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
    exec: function(editor) {
      that.getType();
    }
  });
  // Add gutter to the help overlay
  this._editor.renderer.once('afterRender', function(ev) {
    var $gutter = $('#editor .ace_gutter');
    $gutter.attr('data-intro', 'Double click on a line to trace its values whenever it is reached');
    $gutter.attr('data-position', 'topright');
  });
  // Prevent gutter clicks from selecting text.
  this._editor.on('guttermousedown', function(ev) {
    ev.stop();
  });
  // Set lines to watch by clicking on the gutter, based on
  // https://groups.google.com/forum/#!msg/ace-discuss/sfGv4tRWZdY/ca1LuolbLnAJ
  this._editor.on('gutterdblclick', function(ev) {
    var target = ev.domEvent.target;
    if (target.className.indexOf('ace_gutter-cell') == -1) return;

    var row = ev.getDocumentPosition().row;
    if (target.className.indexOf('ace_breakpoint') == -1) {
      that._editor.session.setBreakpoint(row);
    } else {
      that._editor.session.clearBreakpoint(row);
    }
  });
  this._editor.session.doc.on('change', function(ev) {
    var len, firstRow, f1;
    if (ev.end.row == ev.start.row) return;
    if (ev.action == 'insert') {
      len = ev.end.row - ev.start.row;
      firstRow = ev.start.column == 0 ? ev.start.row: ev.start.row + 1;
    } else if (ev.action == 'remove') {
      len = ev.start.row - ev.end.row
      firstRow = ev.start.row;
    }

    if (len > 0) {
      var args = Array(len);
      args.unshift(firstRow, 0);
      this.$breakpoints.splice.apply(this.$breakpoints, args);
    } else if (len < 0) {
      var rem = this.$breakpoints.splice(firstRow + 1, -len);
      if (!this.$breakpoints[firstRow]) {
        for (var oldBP in rem) {
          if (rem[oldBP]) {
            this.$breakpoints[firstRow] = rem[oldBP]
            break
          }
        }
      }
    }
  }.bind(this._editor.session));

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
      that._reloadInput();
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

    // Evaluate on Cmd-/Ctrl-S anywhere in the window.
    $(document).keydown(function(ev) {
      if (ev.which == 83 && (ev.metaKey || ev.ctrlKey)) {
        ev.preventDefault();
        that.traceInput(true);
      }
    });
  }
}

// `cb` is passed whether the input was reloaded, and whether the input has
// errors.
LiveHaskell.prototype._reloadInput = function(cb, force) {
  if (!force && !this._isInputChanged()) {
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
    that._savedEditCount = editCount;
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

LiveHaskell.prototype.getType = function() {
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
      col_start: range.start.column + 1,
      line_end: range.end.row + 1,
      col_end: range.end.column + 1,
      text: that._editor.session.getTextRange(range),
    }, function(result) {
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

LiveHaskell.prototype.traceInput = function(force) {
  var editCount = this._editCounter;
  var that = this;
  this._reloadInput(function(isReloaded, hasErrors) {
    if (hasErrors) return;
    $.post('trace', {
      filename: that._filename,
      script: that._commander.getInput(),
    }, function(result) {
      // TODO errors
      console.log(result);
      that._evaluatedEditCount = editCount;
      that._tracedResult = result;
      that.setOutput(result.output);
      that._showTrace(that._tracedResult);
    });
  }, force);
}

LiveHaskell.prototype._showTrace = function(traceOutput) {
  var breakpoints = this._editor.session.$breakpoints;
  var steps = traceOutput.steps;
  var aceInfo = [];
  for (var i = 0; i < steps.length; i++) {
    var step = steps[i];
    var row = step[0] - 1;  // 0 indexed
    if (breakpoints[row] == null) continue;
    var values = step[1];
    aceInfo.push({
      row: row,
      text: JSON.stringify(values),
      type: 'info',
    });
  }
  this._editor.session.setAnnotations(aceInfo);
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

// Return true if the current buffer is not the same as what was most recently
// reloaded.
LiveHaskell.prototype._isInputChanged = function() {
  return this._editCounter !== this._savedEditCount;
}
