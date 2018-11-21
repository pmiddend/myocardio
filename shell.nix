{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, base, bytestring
      , cabal-install, hlint, stdenv, stylish-haskell, text, time
      , xdg-basedir
      }:
      mkDerivation {
        pname = "myocardio";
        version = "1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson aeson-pretty base bytestring text time xdg-basedir
        ];
        executableToolDepends = [ cabal-install hlint stylish-haskell ];
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
