with import <nixpkgs> { };
haskell.lib.buildStackProject {
   name = "yesod-serv";
   ghc = haskell.packages.ghc801.ghc;
   buildInputs = [ git openssl ];
#      [ zlib glib autoreconfHook stack ];
   LANG = "en_US.UTF-8";
 }
