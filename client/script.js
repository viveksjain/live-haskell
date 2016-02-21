$(document).ready(function() {
  editor = ace.edit('editor');
  editor.setTheme('ace/theme/monokai');
  editor.getSession().setMode('ace/mode/haskell');
  editor.commands.addCommand({
    name: 'evaluate',
    bindKey: {win: 'Ctrl-Enter', mac: 'Command-Enter'},
    exec: function(editor) {
      evaluateCode();
    }
  });

  output = ace.edit('output');
  output.setOptions({
    readOnly: true,
    highlightActiveLine: false,
    showGutter: false
  });
  // Hack - hide cursor
  output.renderer.$cursorLayer.element.style.opacity = 0;
});

$(document).keyup(function(ev) {
  // Enter key
  if (ev.which == 13) {
    evaluateCode();
  }
});

function evaluateCode() {
  // $.post();
}
