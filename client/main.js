$(document).ready(function () {
  // Purposely a global so it can be used anywhere, and to allow for
  // easier debugging.
  live_haskell = new LiveHaskell();
  new FileSelector(live_haskell);
});
