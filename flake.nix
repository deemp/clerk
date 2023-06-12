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
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      inherit (pkgs.lib.attrsets) genAttrs mapAttrs';
      inherit (inputs.my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.drv-tools.functions.${system}) mkShellApps mkBin mkShellApp mapGenAttrs mapStrGenAttrs;
      inherit (inputs.my-codium.configs.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
      inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
      inherit (inputs.my-devshell.functions.${system}) mkCommands mkRunCommands mkShell;
      inherit (inputs.workflows.functions.${system}) writeWorkflow;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
      inherit (inputs) workflows;

      packageName = "clerk";

      ghcVersion = "928";
      override =
        let inherit (pkgs.haskell.lib) doJailbreak dontCheck overrideCabal; in
        {
          overrides = self: super: {
            "${packageName}" = overrideCabal (super.callCabal2nix packageName ./. { })
              (x: {
                librarySystemDepends = [
                  pkgs.zlib
                  pkgs.expat
                  pkgs.bzip2
                ] ++ (x.librarySystemDepends or [ ]);
                libraryHaskellDepends = [
                  super.xlsx_1_1_0_1
                ] ++ (x.libraryHaskellDepends or [ ]);
              });
          };
        };
      inherit (toolsGHC {
        version = ghcVersion;
        inherit override;
        packages = (ps: [ ps.${packageName} ]);
      })
        cabal ghcid hpack ghc implicit-hie hls;

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
                text = "${cabal}/bin/cabal run example${x}";
                description = "Get `example-${x}.xlsx`";
              };
            }) [ 1 2 3 4 ]
          )
        );

      buildPrefix = "buildWithGHC";
      ghcVersions = [
        "8107"
        "902"
        "928"
        # "945"
        # "962"
      ];

      cabalBuild = mkShellApps
        (mapGenAttrs
          (version: {
            "${buildPrefix}${version}" =
              let inherit (toolsGHC {
                inherit version override; packages = (ps: [ ps.${packageName} ]);
              }) cabal; in
              {
                name = "cabal-build";
                text = "${cabal}/bin/cabal v1-build ${packageName}";
              };
          })
          ghcVersions
        )
      ;

      tools = [
        cabal
        hpack
        hls
        ghcid
        implicit-hie
        pkgs.haskellPackages.fourmolu_0_12_0_0
      ];

      packages = {
        codium = mkCodium {
          extensions = { inherit (extensions) nix haskell misc github markdown; };
          runtimeDependencies = tools;
        };

        writeSettings = writeSettingsJSON (settingsCommonNix // {
          inherit (settingsNix) haskell; extra = {
          "haskell.plugin.fourmolu.config.external" = true;
        };
        });

        inherit (mkFlakesTools [ "." ]) updateLocks pushToCachix;

        writeWorkflows = writeWorkflow "ci" (
          import ./nix-files/workflow.nix {
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
            ++ mkRunCommands "test" { inherit (packages) example2 example3; }
            ++ mkRunCommands "scripts" { inherit (packages) writeDocs; };
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
