{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    my-devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
    lima.url = "github:deemp/flakes?dir=lima";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self
    , flake-utils
    , flakes-tools
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , my-devshell
    , workflows
    , lima
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (pkgs.lib.attrsets) genAttrs mapAttrs';
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkShellApps mkBin mkShellApp mapGenAttrs mapStrGenAttrs;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (my-devshell.functions.${system}) mkCommands mkRunCommands mkShell;
      inherit (workflows.functions.${system}) writeWorkflow run expr mkAccessors genAttrsId;


      ghcVersion = "92";
      override =
        let inherit (pkgs.haskell.lib) doJailbreak dontCheck; in
        {
          overrides = self: super: {
            clerk = pkgs.haskell.lib.overrideCabal (super.callCabal2nix "clerk" ./. { })
              (x: {
                librarySystemDepends = [
                  pkgs.zlib
                  pkgs.expat
                  pkgs.bzip2
                ] ++ (x.librarySystemDepends or [ ]);
                testHaskellDepends = [
                  (super.callCabal2nix "lima" "${lima.outPath}/lima" { })
                ] ++ (x.testHaskellDepends or [ ]);
              });
          };
        };
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        packages = (ps: [ ps.clerk ]);
      })
        stack hls cabal ghcid hpack ghc;

      scripts =
        mkShellApps (
          {
            writeDocs = {
              text = ''${cabal}/bin/cabal v1-test docs'';
            };
          }
          //
          (mapStrGenAttrs
            (x: {
              "example${x}" = {
                text = "(cd example && nix run .#example${x}) && mv example/example-${x}.xlsx .";
                description = "Get `example-${x}.xlsx`";
              };
            }) [ 1 2 ]
          )
        );

      buildPrefix = "buildWithGHC";
      ghcVersions = [ "8107" "902" "925" ];

      cabalBuild = mkShellApps
        (mapGenAttrs
          (x: {
            "${buildPrefix}${x}" =
              let inherit (toolsGHC x override (ps: [ ps.clerk ]) [ ]) cabal; in
              {
                name = "cabal-build";
                text = "${cabal}/bin/cabal v1-build";
              };
          })
          ghcVersions
        )
      ;

      tools = [
        cabal
        hpack
        hls
        ghc
      ];

      packages = {
        codium = mkCodium {
          extensions = { inherit (extensions) nix haskell misc github markdown; };
          runtimeDependencies = tools;
        };

        writeSettings = writeSettingsJSON {
          inherit (settingsNix) haskell todo-tree files editor gitlens
            git nix-ide workbench markdown-all-in-one markdown-language-features;
        };

        inherit (mkFlakesTools [ "." "example" ]) updateLocks pushToCachix;

        writeWorkflows = writeWorkflow "ci" (
          import ./nix-dev/workflow.nix {
            inherit system workflows scripts buildPrefix ghcVersions;
          }
        );
      } // cabalBuild // scripts;

      devShells = {
        default = mkShell {
          packages = tools;
          bash.extra = ''export LANG=C'';
          commands =
            mkCommands "tools" tools
            ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
            ++ mkRunCommands "infra" { inherit (packages) writeWorkflows; }
            ++ mkRunCommands "test" { inherit (packages) example1 example2; };
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
