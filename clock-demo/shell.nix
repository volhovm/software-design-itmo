with import <nixpkgs> { };
haskell.lib.buildStackProject {
   name = "clock-demo";
   ghc = haskell.packages.ghc802.ghc;
   buildInputs = [ git openssl gmp ];
#      [ zlib glib autoreconfHook stack ];
   LANG = "en_US.UTF-8";
 }
