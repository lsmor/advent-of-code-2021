# advent-of-code

The structure of this repo is:
```
./solutions
  |- day-1.hs
  |- day-2.hs
  |- ...
stack-configuration-files
```

the `package.yaml` contains one executable per day. To run a solution just run `stack run day-X`. The advantage of doing this way is that we have good integration with haskell language server **and** we have one simple haskell file per solution. Another approach would be to use `stack script`, the problem is that `hls` does not integrate well with that `stack` feature, hence is better to create a project with a resolver so the language server can pick up the `ghc` version correctly. Of course, having a global `ghc` instalation is an option too, but I'd prefer to not go that way.
