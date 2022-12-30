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
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
  };
  outputs =
    { self
    , flake-utils
    , flakes-tools
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
      inherit (drv-tools.functions.${system}) mkBin withAttrs withMan withDescription mkShellApp;
      inherit (drv-tools.configs.${system}) man;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (devshell.functions.${system}) mkCommands mkShell;
      inherit (haskell-tools.functions.${system}) toolsGHC;

      # set ghc version
      ghcVersion_ = "92";
      inherit (toolsGHC ghcVersion_)
        hls cabal staticExecutable
        implicit-hie ghcid callCabal hpack;

      # my app
      myPackageName = "nix-managed";

      # --- non-haskell deps ---
      myPackageDeps = [
        pkgs.zlib
        pkgs.expat
        pkgs.bzip2
      ];


      # --- cabal shell ---
      inherit (builtins) concatLists attrValues;

      # haskell packages with overrides
      # see https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/
      hp = pkgs.haskell.packages."ghc${ghcVersion_}".override {
        overrides = self: super: {
          myPackage = callCabal myPackageName ./. { };
        };
      };

      cabalShell =
        hp.shellFor {
          packages = ps: [ ps.myPackage ];
          nativeBuildInputs = myPackageDeps;
          withHoogle = true;
          # get other tools with names
          shellHook = ''
            nix develop .#tools
          '';
        };

      codiumTools = [ hpack ghcid cabal ];

      # VSCodium with dev tools
      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools ++ [ hls ];
      };

      # --- all dev tools ---
      tools = codiumTools ++ [ codium ];

      # --- flakes tools ---
      flakesTools = mkFlakesTools [ "." ];

      # --- codium ---
      # what to write in settings.json
      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };


      # --- default shell ---
      defaultShell = mkShell
        {
          packages = tools;
          bash.extra = "";
          commands = mkCommands "tools" tools;
        };
      # TODO add flags
      #         "$everything": -haddock
      # "$locals": -Wall

    in
    {
      packages = {
        inherit (flakesTools) updateLocks pushToCachix;
      };

      devShells = {
        # --- devshell with dev tools ---
        # runs nix-packaged app
        default = cabalShell;

        # --- shell for cabal ---
        # runs cabal
        tools = defaultShell;
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
