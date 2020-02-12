{ pkgs ? (import ./nix/nixpkgs) }:

let
  compiler = "ghc865";
  source = pkgs.lib.sourceByRegex ./. [
    "^.*\.md$"
    "^distributed-closure\.cabal$"
    "^examples.*$"
    "^src.*$"
    "^tests.*$"
  ];
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = distributedClosureOverlay;
  };
  distributedClosureOverlay = self: super: {
    "distributed-closure" = pkgs.haskell.lib.enableCabalFlag
      (super.callCabal2nix "distributed-closure" source { }) "dev";
  };
in {
  distributed-closure = haskellPackages.distributed-closure;
  shell = haskellPackages.shellFor {
    packages = ps: [
      ps.distributed-closure
    ];
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  };
}
