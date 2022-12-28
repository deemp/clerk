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
    my-lima.url = "github:deemp/flakes?dir=lima";
    workflows.url = "github:deemp/flakes?dir=workflows";
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
    , my-lima
    , workflows
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (builtins) foldl';
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkBinName withAttrs mkShellApps mkBin;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      devshell = my-devshell.devshell.${system};
      lima = my-lima.packages.${system}.default;
      inherit (my-devshell.functions.${system}) mkCommands;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (workflows.functions.${system}) writeWorkflow run nixCI_ stepsIf expr mkAccessors genId;
      inherit (workflows.configs.${system}) steps os oss;
      inherit (toolsGHC "92") stack hls cabal;

      appPackages = [
        pkgs.zlib
        pkgs.expat
        pkgs.bzip2
      ];

      ghcVersions = [ "865Binary" "88" "810" "90" "92" "94" ];
      ghcs = map (x: "ghc${x}") ghcVersions;

      buildScripts = mkShellApps (
        foldl'
          (acc: ghc: acc // (
            let
              name = "cabal-ghc${ghc}";
              builder = (toolsGHC ghc).cabal appPackages;
            in
            {
              "${name}" = {
                inherit name;
                text = ''
                  ${mkBin builder} build
                '';
                runtimeInputs = [ builder ];
              };
            }
          ))
          { }
          ghcVersions
      );

      names = withAttrs (workflows.configs.${system}).names (mkAccessors ({ matrix = genId "ghc"; }));

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      scripts = mkShellApps {
        updateReadme = {
          text = ''
            ${mkBin lima} toMd src/Example.lhs
            cat README/Intro.md > doc.md
            printf "\n" >> doc.md
            cat src/Example.lhs.md >> doc.md
            printf "\n" >> doc.md
            cat README/Conclusion.md >> doc.md
            rm src/Example.lhs.md
            mv doc.md README.md
          '';
          runtimeInputs = [ lima ];
          description = "Write README.md";
        };
      };

      codiumTools = [
        stack
        writeSettings
        lima
        cabal
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools ++ [ hls ];
      };

      tools = codiumTools ++ [ codium ] ++ (builtins.attrValues scripts);
      flakesTools = mkFlakesTools [ "." ];

      nixCI = withAttrs
        (
          nixCI_ [
            {
              name = "Update README.md";
              run = run.runExecutableAndCommit "updateReadme" "Update README.md";
              "if" = "${names.matrix.os} == '${os.ubuntu-20}'";
            }
          ])
        {
          jobs = {
            build = {
              name = "Build";
              strategy.matrix = {
                os = oss;
                ghc = ghcs;
              };
              runs-on = expr names.matrix.os;
              steps = [

              ];
            };
          };
        };

    in
    {
      packages = {
        default = codium;
        inherit (flakesTools) updateLocks pushToCachix;
        writeWorkflows = writeWorkflow "ci" nixCI;
      } // scripts // buildScripts;

      devShells.default = devshell.mkShell
        {
          packages = tools;
          bash.extra = ''printf "Hello, world!\n"'';
          commands = mkCommands "tools" tools;
        };

      stack-shell = { ghcVersion }:

        pkgs.haskell.lib.buildStackProject {
          name = "stack-shell";

          ghc = pkgs.haskell.compiler.${ghcVersion};

          buildInputs = appPackages;
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
