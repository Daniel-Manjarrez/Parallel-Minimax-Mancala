# Parallel-Minimax-Mancala
A parallel functional programming project that showcases a parallelized minimax mancala solver in Haskell

```ghc -O2 -threaded -rtsopts -with-rtsopts=-N ParaMancala3.hs```

```./ParaMancala3 12 6 +RTS -N6``` <- arg[1] is depth, arg[2] is a parallel depth past which algorithm runs fully sequentially
