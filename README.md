# Littercoin Open Source Repository

![Littercoin User Journey](/images/littercoin_user_journey.png)

![Littercoin High Level Design](/images/littercoin_design.png)



## Setting up

### Cabal+Nix build

Use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

## The Plutus Application Backend (PAB) Simulator

We have provided an example PAB simulator application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/ARCHITECTURE.adoc).


1. Build the PAB executable:

```
cabal build littercoin-pab
```

2. Run the PAB binary:

```
cabal exec littercoin-pab
````



## Support/Issues/Community

Issues can be filed in the [GitHub Issue tracker](tbd).

For more interactive discussion, you can join the [Discord](tbd).

Thanks!
