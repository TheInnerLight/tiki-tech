let
  myNixPkgs = import <nixpkgs> {};
in
myNixPkgs.mkShell {
  nativeBuildInputs = with myNixPkgs; [
    cabal-install # terminal app cabal
    haskell.compiler.ghc94 # Haskell compiler
    haskell.packages.ghc94.haskell-language-server
    gitMinimal
    SDL2
    SDL2_ttf
    SDL2_gfx
    pkg-config
  ];
}