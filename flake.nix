{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
    feedback.url = "github:NorfairKing/feedback";
  };

  outputs = { self, nixpkgs, haskellNix, flakeUtils, feedback }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flakeUtils.lib.eachSystem supportedSystems (system:
        let
          overlays = [
            haskellNix.overlay
            (final: prev: {
              elocryptProject = final.haskell-nix.cabalProject' {
                src = ./.;
                compiler-nix-name = "ghc962";
                name = "elocrypt";

                shell = {
                  tools = {
                    cabal = "latest";
                    hlint = "latest";
                    haskell-language-server = "latest";
                    fourmolu = "0.13.1.0";
                  };

                  nativeBuildInputs = [pkgs.haskellPackages.weeder];

                  withHoogle = true;
                };
              };
            })
          ];

          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          flake = pkgs.elocryptProject.flake {
          };
        in
          pkgs.lib.recursiveUpdate flake {
            packages.default = flake.packages."elocrypt:exe:elocrypt";
          });


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
    experimental-features = ["nix-command flakes"];
  };
}
