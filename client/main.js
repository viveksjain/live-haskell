var DEBUG = false;

$(document).ready(function () {
  // Purposely a global to allow for easier debugging.
  live_haskell = new LiveHaskell();
  new FileSelector(live_haskell);
  new Refresher(live_haskell);
  new Commander(live_haskell);

  $('#help').click(show_help);

  if (!localStorage.seen) {
    localStorage.seen = true;
    show_help();
  }
});
