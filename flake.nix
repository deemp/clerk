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
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (drv-tools.functions.${system}) mkBinName withAttrs;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      devshell = my-devshell.devshell.${system};
      lima = my-lima.packages.${system}.default;
      inherit (my-devshell.functions.${system}) mkCommands;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (toolsGHC "92") stack hls ghc implicit-hie ghcid;

      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      codiumTools = [
        implicit-hie
        ghcid
        stack
        writeSettings
        ghc
        lima
      ];

      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools ++ [ hls ];
      };

      tools = codiumTools ++ [ codium ];
      flakesTools = mkFlakesTools [ "." ];
    in
    {
      packages = {
        default = codium;
        inherit (flakesTools) updateLocks pushToCachix;
      };

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

          buildInputs = [
            pkgs.zlib
            pkgs.expat
            pkgs.bzip2
            pkgs.xdg-utils
            pkgs.firefox
          ];
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
