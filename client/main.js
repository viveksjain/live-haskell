var DEBUG = false;

$(document).ready(function () {
  // Purposely a global to allow for easier debugging.
  liveHaskell = new LiveHaskell();
  new FileSelector(liveHaskell);
  new Refresher(liveHaskell);
  new Commander(liveHaskell);

  $('#help').click(showHelp);
});
