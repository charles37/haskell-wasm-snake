Small experiment using the new WASM capabilities of GHC.

```shell
nix run .#update
nix run .#build
python -m http.server --directory dist
```

 The snake game reads keyboard events (JavaScript), computes the GameState (Haskell), 
 and renders the GameState to the console (JavaScript). 

https://gitlab.haskell.org/ghc/ghc-wasm-meta

https://github.com/tweag/ormolu/tree/master/ormolu-live
