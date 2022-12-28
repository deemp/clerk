{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc92", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, data-default
      , lens, lib, mtl, text, time, transformers, xlsx
      }:
      mkDerivation {
        pname = "clerk";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring containers data-default lens mtl text time
          transformers xlsx
        ];
        executableHaskellDepends = [
          base bytestring containers data-default lens mtl text time
          transformers xlsx
        ];
        executableSystemDepends = [
          pkgs.zlib
        ];
        homepage = "https://github.com/deemp/clerk#readme";
        license = lib.licenses.bsd3;
        mainProgram = "clerk";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
