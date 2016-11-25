with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "yesod-serv";
     ghc = hsPkgs.ghc;
#     buildInputs =
#       [ zlib glib autoreconfHook stack openssl ];
     LANG = "en_US.UTF-8";
  }
