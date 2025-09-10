{ inputs, system }:

let
  inherit (inputs) nixpkgs haskell-nix pre-commit-hooks;
  inherit (pkgs) lib;

  pkgs = import nixpkgs {
    inherit system;
    config = haskell-nix.config;
    overlays = [ haskell-nix.overlay ];
  };

  project = pkgs.haskell-nix.cabalProject' ({ ... }: {
    name = "cardano-ledger-release-tool";
    src = lib.cleanSource ../.;
    compiler-nix-name = lib.mkDefault "ghc967";
    flake.variants = {
      ghc98.compiler-nix-name = "ghc984";
      ghc910.compiler-nix-name = "ghc9102";
      ghc912.compiler-nix-name = "ghc9122";
    };
  });

  static = pkgs.symlinkJoin {
    name = "${project.args.name}-static";
    paths =
      builtins.concatMap
        (p: lib.attrsets.attrValues p.components.exes)
        (builtins.filter
          (p: p ? "isLocal" && p.isLocal)
          (lib.attrsets.attrValues project.projectCross.musl64.hsPkgs));
  };

in

lib.attrsets.recursiveUpdate (project.flake { }) {
  packages.default = static;
}
