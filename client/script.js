$(document).ready(function () {
  // These are purposely globals so they can be used anywhere, and to allow for
  // easier debugging.
  editor = ace.edit('editor');
  editor.$blockScrolling = Infinity;  // Hide Ace error messages
  editor.setTheme('ace/theme/monokai');
  editor.getSession().setMode('ace/mode/haskell');
  editor.commands.addCommand({
    name: 'evaluate',
    bindKey: {win: 'Ctrl-Enter', mac: 'Command-Enter'},
    exec: function (editor) {
      evaluateCode();
    }
  });
  editor.session.setTabSize(2);

  output = ace.edit('output');
  output.$blockScrolling = Infinity;  // Hide Ace error messages
  output.setOptions({
    readOnly: true,
    highlightActiveLine: false,
    showGutter: false
  });
  // Hack - hide cursor
  output.renderer.$cursorLayer.element.style.opacity = 0;
});

$(document).keyup(function (ev) {
  // Enter key
  if (ev.which == 13 && !(ev.metaKey || ev.ctrlKey)) {
    evaluateCode();
  }
});

function evaluateCode() {
  $.post('evaluate', {script: editor.getValue()}, function (result) {
    console.log(result);
    output.setValue(result.output || result.error, 1);

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
    editor.getSession().setAnnotations(aceErrors);
  });
}
