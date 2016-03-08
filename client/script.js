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
