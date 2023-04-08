# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "myocardio";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "myocardio";
      in
      {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            vty = (haskellPackages.vty.override {
              terminfo = haskellPackages.terminfo_0_4_1_5;
            });
          };

        packages."${packageName}-static" = pkgs.pkgsStatic.haskellPackages.callCabal2nix packageName self rec {
          vty = (pkgs.pkgsStatic.haskellPackages.vty.override {
            terminfo = pkgs.pkgsStatic.haskellPackages.terminfo_0_4_1_5;
          });
        };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
