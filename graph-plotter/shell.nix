with import <nixpkgs> { };
haskell.lib.buildStackProject {
   name = "graph-plotter";
   ghc = haskell.packages.ghc801.ghcWithPackages (p: with p; [ gtk2hs-buildtools ]);
   buildInputs = [ git openssh zlib cairo gnome2.gtk glib glew freeglut mesa ];
   LANG = "en_US.UTF-8";
}
