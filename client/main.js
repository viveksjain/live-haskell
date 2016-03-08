$(document).ready(function () {
  // Purposely a global to allow for easier debugging.
  live_haskell = new LiveHaskell();
  new FileSelector(live_haskell);
  new Refresher(live_haskell);
});
