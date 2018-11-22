{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, base, bifunctors
      , brittany, bytestring, cabal-install, composition, directory
      , hlint, hspec, quickcheck-instances, semigroups, stdenv, text
      , time, xdg-basedir
      }:
      mkDerivation {
        pname = "myocardio";
        version = "1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson aeson-pretty base bifunctors bytestring composition directory
          semigroups text time xdg-basedir
        ];
        libraryToolDepends = [ brittany cabal-install hlint ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base hspec quickcheck-instances ];
        homepage = "https://github.com/pmiddend/myocardio";
        description = "An ncurses tool magically generating workout plans";
        license = stdenv.lib.licenses.lgpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
