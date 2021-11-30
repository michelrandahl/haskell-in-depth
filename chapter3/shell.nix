{ nixpkgs ? import <old> {}, compiler ? "ghc865", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-colonnade, blaze-html, bytestring
      , cassava, Chart, Chart-diagrams, colonnade, fmt, lib
      , optparse-applicative, text, time
      }:
      mkDerivation {
        pname = "chapter3";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base blaze-colonnade blaze-html bytestring cassava Chart
          Chart-diagrams colonnade fmt optparse-applicative text time
        ];
        description = "Project synopsis";
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
