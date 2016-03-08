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
  this._$elem = $('#refresh');
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
    // Ensure no changes between the text that was evaluated and now.
    if (!this._live_haskell.isOutputUpdated()) {
      this._setRefreshable(false);
    }
  }
};
