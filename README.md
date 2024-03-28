# lambda
Lambda calculus calculator. The predecessor to [some later work](https://github.com/kyubxy/LambdaCalc)

## What does it do
It evaluates lambda calculus expressions using eager semantics (adjustable later maybe). Currently it can only parse and evaluate what its given in an
interactive prompt but eventually it'll be able to read files.

## How do I use it
I built this program using cabal

```
cabal v2-install
cabal build
cabal run
```

When using the interactive prompt, use `/` (forward slash) for lambdas.

## Lambda calculus is a very easy thing to check and do proofs on, why test instead of using a theorom proover like coq?
idk, maybe later? testing is way easier.
