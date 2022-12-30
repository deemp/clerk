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
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkBinName withAttrs mkShellApps mkBin;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      devshell = my-devshell.devshell.${system};
      lima = my-lima.packages.${system}.default;
      inherit (my-devshell.functions.${system}) mkCommands;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (workflows.functions.${system}) writeWorkflow run nixCI_ stepsIf expr;
      inherit (workflows.configs.${system}) steps names os;

      ghcVersion = "92";
      inherit (toolsGHC ghcVersion) stack hls cabal ghcid hpack;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      scripts = mkShellApps {
        writeReadme = {
          text =
            let lhs = "example/app/Main.lhs"; in
            ''
              ${mkBin lima} toMd ${lhs}
              cat README/Intro.md > doc.md
              printf "\n" >> doc.md
              cat ${lhs}.md >> doc.md
              printf "\n" >> doc.md
              cat README/Conclusion.md >> doc.md
              rm ${lhs}.md
              mv doc.md README.md
            '';
          runtimeInputs = [ lima ];
          description = "Write README.md";
        };
      };

      codiumTools = [
        cabal
        writeSettings
        lima
        hpack
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools ++ [ hls ];
      };

      tools = codiumTools ++ [ codium ] ++ (builtins.attrValues scripts);
      flakesTools = mkFlakesTools [ "." ];

      nixCI = nixCI_ [
        {
          name = "Write README.md";
          run = run.runExecutableAndCommit scripts.writeReadme.pname "Write README.md";
          "if" = "${names.matrix.os} == '${os.ubuntu-20}'";
        }
      ];

      inherit (builtins) concatLists attrValues;
      inherit (pkgs.haskell.lib) doJailbreak dontCheck;
      hp = pkgs.haskell.packages."ghc${ghcVersion}".override {
        overrides = self: super: {
          clerk = self.callCabal2nix "clerk" ./. { };
          xlsx = dontCheck (doJailbreak super.xlsx);
        };
      };

      cabalShell =
        hp.shellFor {
          packages = ps: [ ps.clerk ];
          nativeBuildInputs = [
            pkgs.zlib
            pkgs.expat
            pkgs.bzip2
          ];
          withHoogle = true;
          shellHook = ''
            nix develop .#tools
          '';
        };
      # TODO add flags

      # "$everything": -haddock
      # "$locals": -Wall
    in
    {
      packages = {
        default = codium;
        inherit (flakesTools) updateLocks pushToCachix;
        writeWorkflows = writeWorkflow "ci" nixCI;
      } // scripts;

      devShells =
        {
          default = cabalShell;
          tools = devshell.mkShell
            {
              packages = tools;
              bash.extra = '''';
              commands = mkCommands "tools" tools;
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
