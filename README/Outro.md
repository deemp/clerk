## Contribute

This project provides a dev environment via a `Nix` flake.

1. With [flakes enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes), run:

    ```console
    nix develop
    cabal build
    ```

1. This `README.md` is generated from several files. If you edit them, re-generate it.

    ```console
    cabal test docs
    ```

1. (Optionally) Start `VSCodium` with `Haskell` extensions.

    1. Write settings and run `VSCodium`.

        ```console
        nix run .#writeSettings
        nix run .#codium .
        ```

    1. Open a `Haskell` file. `Haskell Language Server` should soon start giving you hints.

1. Study these links if you'd like to learn more about the tools used in this flake:

    - [Prerequisites](https://github.com/deemp/flakes#prerequisites)
    - `Haskell` project [template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme)
    - [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)

1. If `Haskell Language Server` doesn't want to run code in `-- >>>` comments:
   1. Check the Output of `HLS`.
   1. Find there a directory name containing `hie-bios`.
   1. Remove the `hie-bios/dist-clerk-*` directory.
