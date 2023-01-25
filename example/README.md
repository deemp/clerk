# clerk example

Describe a table for calculation of pressure from given volume data and constants.

## Prerequisites

- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)

## Quick start

### cabal

```console
cabal update
cabal run
```

### nix + cabal

1. [Install Nix](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, run

    ```console
    direnv allow
    nix develop
    cabal run
    ```

1. Optionally, start `VSCodium` with `Haskell` extensions.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. Open a `Haskell` file `app/Main.hs` and hover over a function.

1. Wait until the `Haskell Language Server` starts giving you type info.
