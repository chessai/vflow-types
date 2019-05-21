{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  vflow-types = (
    with rec {
      vflow-typesSource = pkgs.lib.cleanSource ../.;
      vflow-typesBasic  = self.callCabal2nix "vflow-types" vflow-typesSource { };
    };
    overrideCabal vflow-typesBasic (old: {
    })
  );
}
