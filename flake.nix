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
      inherit (drv-tools.functions.${system}) mkBinName withAttrs mkShellApps mkBin mkShellApp;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (my-devshell.functions.${system}) mkCommands mkShell;
      inherit (workflows.functions.${system})
        writeWorkflow run stepsIf expr
        mkAccessors genAttrsId;
      inherit (workflows.configs.${system}) steps os oss nixCI;

      ghcVersion = "92";
      override =
        let inherit (pkgs.haskell.lib) doJailbreak dontCheck; in
        {
          overrides = self: super: {
            clerk = pkgs.haskell.lib.overrideCabal (super.callCabal2nix "clerk" ./. { })
              (_: {
                librarySystemDepends = [
                  pkgs.zlib
                  pkgs.expat
                  pkgs.bzip2
                ];
              });
            lima = super.callCabal2nix "lima" "${lima.outPath}/lima" { };
          };
        };
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (toolsGHC ghcVersion override (ps: [ ps.clerk ]) [ ])
        stack hls cabal ghcid hpack;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      tools = [
        cabal
        hpack
        hls
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = tools;
      };

      flakesTools = mkFlakesTools [ "." "example" ];

      ghcVersions = [ "8107" "902" "925" ];

      buildPrefix = "buildWithGHC";
      scripts = {
        cabalBuild = mapAttrs'
          (
            name: value: { name = "${buildPrefix}${name}"; inherit value; }
          )
          (genAttrs ghcVersions (ghcVersion_:
            let inherit (toolsGHC ghcVersion_ override (ps: [ ps.clerk ]) [ ]) cabal; in
            mkShellApp {
              name = "cabal-build";
              text = "cabal build";
              runtimeInputs = [ cabal ];
            }));
      } // (
        mkShellApps {
          writeDocs = {
            text = ''cabal test docs'';
            runtimeInputs = [ cabal ];
          };
        });

      names = mkAccessors {
        matrix = genAttrsId [ "os" "ghc" ];
      };

      workflow =
        let
          job1 = "_1_nix_ci";
          job2 = "_2_build_with_ghc";
          job3 = "_3_push_to_cachix";
        in
        nixCI // {
          jobs = {
            "${job1}" = {
              name = "Update flake locks and README.md";
              runs-on = os.ubuntu-20;
              steps =
                [
                  steps.checkout
                  steps.installNix
                  steps.configGitAsGHActions
                  steps.updateLocksAndCommit
                  {
                    name = "Write docs";
                    run = run.nixRunAndCommit scripts.writeDocs.pname "Write docs";
                  }
                ];
            };
            "${job2}" = {
              name = "Build with GHC";
              strategy.matrix.ghc = ghcVersions;
              needs = job1;
              runs-on = os.ubuntu-20;
              steps = [
                steps.checkout
                steps.installNix
                {
                  name = "Pull repo";
                  run = "git pull --rebase --autostash";
                }
                (
                  let ghc = expr names.matrix.ghc; in
                  {
                    name = "Build with ghc${ghc}";
                    run = ''nix run .#${buildPrefix}${ghc}'';
                  }
                )
              ];
            };
            "${job3}" = {
              name = "Push to cachix";
              needs = job1;
              strategy.matrix.os = oss;
              runs-on = expr names.matrix.os;
              steps =
                [
                  steps.checkout
                  steps.installNix
                  steps.logInToCachix
                  steps.pushFlakesToCachix
                ];
            };
          };
        };
      writeWorkflows = writeWorkflow "ci" workflow;

      s = builtins.getFlake;
    in
    {
      packages = {
        inherit (flakesTools) updateLocks pushToCachix;
        inherit codium writeWorkflows;
        inherit (scripts) writeDocs;
      } // scripts.cabalBuild;

      devShells =
        {
          default = mkShell
            {
              packages = tools;
              bash.extra = ''export LANG=C'';
              commands = (mkCommands "tools" tools) ++ [
                {
                  name = "nix run .#codium .";
                  help = codium.meta.description;
                  category = "other commands";
                }
                {
                  name = "nix run .#writeSettings";
                  help = writeSettings.meta.description;
                  category = "other commands";
                }
                {
                  name = "nix run .#writeWorkflows";
                  help = writeWorkflows.meta.description;
                  category = "other commands";
                }
              ];
            };
        };
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
