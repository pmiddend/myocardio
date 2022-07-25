let nixpkgs = import (import nix/sources.nix {}).nixpkgs {  };
    # hp = nixpkgs.pkgsStatic.haskell.compiler.ghc902
    hp = nixpkgs.pkgsStatic.haskellPackages.override {
      overrides = self: super: {
        vty = (super.vty.override {
          terminfo = super.terminfo_0_4_1_5;
        });
      };
    };
in hp.callCabal2nix "myocardio-exe" ./. { }
