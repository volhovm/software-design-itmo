with import <nixpkgs> { };
haskell.lib.buildStackProject {
   name = "graph-plotter";
   ghc = haskell.packages.ghc801.ghc;
   buildInputs = [ git openssh ];
   LANG = "en_US.UTF-8";
}
