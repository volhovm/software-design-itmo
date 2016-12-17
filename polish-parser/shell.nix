with import <nixpkgs> { };
haskell.lib.buildStackProject {
   name = "polish-parser";
   ghc = haskell.packages.ghc801.ghc;
   buildInputs = [ git openssh ];
   LANG = "en_US.UTF-8";
}
