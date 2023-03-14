Snake game that runs in the browser using GHC's WebAssembly backend

```shell
nix run .#update
nix run .#build
python -m http.server --directory dist
```

The app just reads keyboard events (JavaScript), changes game state (Haskell), and renders the game state (JavaScript).

using https://github.com/willmcpherson2/ghc-wasm-experiment as template


https://gitlab.haskell.org/ghc/ghc-wasm-meta

https://github.com/tweag/ormolu/tree/master/ormolu-live
