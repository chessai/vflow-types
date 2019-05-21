{ compiler }:
with rec {
  fetchNixpkgs = import ./fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "4e7693618cfbf901b176c2061bb885aa674f2169";
    sha256 = "096lygipaj9wcpz2c6a13kw5fggshq8xaynh3ka05n1l5zzshdd5";
  };
};
import nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskell.packages.${compiler}.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };
    };
    allowBroken = true;
  };
  overlays = [ ];
}
