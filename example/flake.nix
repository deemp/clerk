{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , devshell
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      # --- imports ---
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkShellApps mapStrGenAttrs;
      inherit (drv-tools.configs.${system}) man;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (devshell.functions.${system}) mkCommands mkRunCommands mkShell;
      inherit (haskell-tools.functions.${system}) toolsGHC;

      # set ghc version
      ghcVersion = "92";
      clerk-example = "clerk-example";

      override = {
        overrides = self: super: {
          "${clerk-example}" = pkgs.haskell.lib.overrideCabal
            (super.callCabal2nix clerk-example ./. { })
            (_: {
              librarySystemDepends = [
                pkgs.zlib
                pkgs.expat
                pkgs.bzip2
              ];
            });
        };
      };

      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        packages = (ps: [ ps.clerk-example ]);
      }) hls hpack cabal;

      tools = [ hpack cabal hls ];

      scripts = mkShellApps (
        mapStrGenAttrs
          (x: {
            "example${x}" = {
              text = "${cabal}/bin/cabal v1-run example-${x}";
              description = "Get `example-${x}.xlsx`";
            };
          }
          ) [ 1 2 ]
      );

      packages = {
        writeSettings = writeSettingsJSON {
          inherit (settingsNix) haskell todo-tree files editor gitlens
            git nix-ide workbench markdown-all-in-one markdown-language-features;
        };

        codium = mkCodium {
          extensions = { inherit (extensions) nix haskell misc github markdown; };
          runtimeDependencies = tools;
        };
      } // scripts;

      devShells = {
        default = mkShell {
          packages = tools;
          bash.extra = "";
          commands =
            mkCommands "tools" tools ++
            mkRunCommands "ide" {
              "codium ." = packages.codium;
              inherit (packages) writeSettings;
            } ++
            mkRunCommands "test" {
              inherit (packages) example1 example2;
            };
        };
      };
    in
    {
      inherit packages devShells;
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
