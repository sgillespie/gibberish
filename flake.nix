{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flakeParts.url = "github:hercules-ci/flake-parts";
    flakeUtils.url = "github:numtide/flake-utils";
    treefmt.url = "github:numtide/treefmt-nix";
  };

  outputs = {
    flakeParts,
    flakeUtils,
    treefmt,
    ...
  } @ inputs:
    flakeParts.lib.mkFlake {inherit inputs;} {
      imports = [
        treefmt.flakeModule

        # Project build via haskell.nix
        ./nix/haskell-project.nix
        # Platform-specific release distribution archives
        ./nix/distribution.nix
        # Tests and static analyzers
        ./nix/checks.nix
        # Source code formatters (treefmt)
        ./nix/formatter.nix
      ];

      systems = flakeUtils.lib.defaultSystems;
    };

  nixConfig = {
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "sgillespie.cachix.org-1:Zgif/WHW2IzHqbMb1z56cMmV5tLAA+zW9d5iB5w/VU4="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    substituters = [
      "https://cache.nixos.org/"
      "https://cache.iog.io"
      "https://sgillespie.cachix.org"
    ];

    allow-import-from-derivation = "true";
  };
}
