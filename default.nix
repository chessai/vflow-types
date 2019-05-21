{ compiler ? "ghc865"
, nixpkgs ? (import ./nix/nixpkgs.nix { inherit compiler; })
}:

with rec {
  drv = nixpkgs.haskellPackages.vflow-types;
};

drv
