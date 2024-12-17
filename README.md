# Parallel-Minimax-Mancala
A parallel functional programming project that showcases a parallelized minimax mancala solver in Haskell

``Requirement``
1. Linux operating system.
2. GHC (Glasgow Haskell Compiler) installed.
3. Cabal build tool installed.

``File Explaination ``
MancalaSolver.hs is a sequential Minimax with Alpha Beta Pruning solution for Mancala Game. 

ParaMancala1.hs attempt parallelize computing the best move at each state without considering parallelized depth

ParaMancala2.hs attempt sequential Minimax in the shallower level while parallelized Minimax in the deeper level.

ParaMancala3.hs attempt parallelized Minimax in the shallower level while sequential Minimax in the deeper level.

ParaMancala4.hs is the final version, applying principal variation of sequential leftmost search. 

``Configuaration and Execuation``

Build the project by this command:

```cabal build```

Execuate the project
``` cabal exec Parallel-Minimax-Mancala <depth> <paradepth>```

Quick test command:

``` cabal test``` 

or

``` cabal test QuickTest```


``Single file execuation Experiment``

```ghc -O2 -threaded -rtsopts -with-rtsopts=-N <..>.hs```

```./ParaMancala3 <depth> <paradepth> +RTS -N6```



