with import <nixpkgs> { };
haskell.lib.buildStackProject {
   name = "react-demo";
   ghc = haskell.packages.ghc802.ghc;
   buildInputs = [ git openssl gmp ];
   LANG = "en_US.UTF-8";
 }
