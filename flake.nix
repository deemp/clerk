{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
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
      inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (inputs.drv-tools.functions.${system}) mkShellApps mkBin mkShellApp mapGenAttrs mapStrGenAttrs;
      inherit (inputs.codium.configs.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
      inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
      inherit (inputs.devshell.functions.${system}) mkCommands mkRunCommands mkShell;
      inherit (inputs.workflows.functions.${system}) writeWorkflow;
      inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
      inherit (inputs) workflows;

      clerk = "clerk";
      convert = "convert";

      ghcVersion = "928";
      override =
        let inherit (pkgs.haskell.lib) doJailbreak dontCheck overrideCabal; in
        {
          overrides = self: super: {
            "${clerk}" = overrideCabal (super.callCabal2nix clerk ./${clerk} { })
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
            "${convert}" = super.callCabal2nix convert ./${convert} { inherit (self) clerk; };
          };
        };

      hpkgs = pkgs.haskell.packages."ghc${ghcVersion}";

      devShells.shellFor = (hpkgs.override override).shellFor {
        packages = ps: [ ps.${clerk} ps.${convert} ];
      };

      tools = [
        pkgs.cabal-install
        pkgs.hpack
        hpkgs.implicit-hie
        hpkgs.haskell-language-server
        pkgs.haskellPackages.fourmolu_0_12_0_0
        pkgs.poetry
      ];

      packages = {
        writeSettings = writeSettingsJSON (settingsCommonNix // {
          inherit (settingsNix) haskell python;
          extra = {
            "haskell.plugin.fourmolu.config.external" = true;
            "python.defaultInterpreterPath" = ".venv/bin/python3";
            "latex-workshop.latex.tools" = [
              {
                "name" = "latexmk";
                "command" = "latexmk";
                "args" = [
                  "-shell-escape"
                  "-synctex=1"
                  "-interaction=nonstopmode"
                  "-file-line-error"
                  "-pdf"
                  "-outdir=%OUTDIR%"
                  "%DOC%"
                ];
                "env" = { };
              }
            ];
          };
        });
      };

      # TODO add script to write docs

      devShells = {
        default = mkShell {
          packages = tools;
          packagesFrom = [ devShells.shellFor ];
          bash.extra = ''export LANG=C'';
          commands = mkCommands "tools" tools;
        };
      };

      packages = { };
    in
    {
      inherit packages devShells;
    });

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}

