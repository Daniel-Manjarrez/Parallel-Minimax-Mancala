# Parallel-Minimax-Mancala
A parallel functional programming project that showcases a parallelized minimax mancala solver in Haskell

```ghc -o Mancala MancalaSolver.hs```

```./MancalaSolver 5 ``` <- arg[1] is depth

```ghc -O2 -threaded -rtsopts -with-rtsopts=-N MancalaSolver.hs```

```./MancalaSolver 6 +RTS -N8``` <- arg[1] is depth