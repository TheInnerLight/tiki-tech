let
  myNixPkgs = import <nixpkgs> {};
in
myNixPkgs.mkShell {
  nativeBuildInputs = with myNixPkgs; [
    cabal-install # terminal app cabal
    haskell.compiler.ghc94 # Haskell compiler
    haskell.packages.ghc94.haskell-language-server
    freetype
    gitMinimal
    glib
    harfbuzz
    pcre2
    SDL2
    SDL2_ttf
    SDL2_gfx
    pkg-config
  ];
}