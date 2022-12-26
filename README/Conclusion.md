## Contribute

### Prerequisites

As this project uses `Nix` for dev environment, study the following prerequisites to set up the project

- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- `Haskell` project [template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme)
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)

Next, run

```sh
nix develop nix-dev/
write-settings-json
codium .
```

and open a `Haskell` file. `HLS` should soon start giving you hints.
