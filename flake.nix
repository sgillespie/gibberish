{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flakeParts.url = "github:hercules-ci/flake-parts";
    flakeUtils.url = "github:numtide/flake-utils";
    treefmt.url = "github:numtide/treefmt-nix";
  };

  outputs = { haskellNix, nixpkgs, flakeParts, flakeUtils, treefmt, ... }@inputs:
    flakeParts.lib.mkFlake { inherit inputs; } ({
      config,
      withSystem,
      moduleWithSystem,
      ...
    }@top: {
      imports = [
        treefmt.flakeModule

        ({
          perSystem = { system, ... }: {
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = [ haskellNix.overlay ];
            };
          };
        })
      ];

      systems = flakeUtils.lib.defaultSystems;

      flake = {
      };

      perSystem = {
        system,
        config,
        pkgs,
        ...
      }: {
        config = 
          let
            haskellProject = (pkgs.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc910";
              name = "gibberish";

              flake.variants.profiled = {
                modules = [{
                  enableLibraryProfiling = true;
                  enableProfiling = true;
                }];
              };

              shell = {
                tools = {
                  cabal = "latest";
                  haskell-language-server = "latest";
                  hp2pretty = "latest";
                };

                nativeBuildInputs = [ hlint ];
                withHoogle = true;

                # We don't need cross platforms in the shell; should speed up evaluation
                crossPlatforms = _: [];
              };
            }).appendOverlays [
              pkgs.haskell-nix.haskellLib.projectOverlays.projectComponents
            ];

            haskellFlake = haskellProject.flake {
              crossPlatforms = p:
               pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 (
                 [p.mingwW64] ++
                 (pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux
                   [p.musl64]));
            };

            hlint = 
              pkgs.haskell-nix.tool 
                haskellProject.args.compiler-nix-name 
                "hlint"
                "latest";

           # Source filtered by .gitignore, for read-only static-analysis checks.
           lintSrc = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

           mkLintCheck = name: nativeBuildInputs: checkPhase:
             pkgs.runCommandLocal "gibberish-${name}-check" { inherit nativeBuildInputs; } ''
               cd ${lintSrc}
               ${checkPhase}
               touch $out
             '';

           cpExesCmd = project:
             let
               inherit (pkgs) lib;
               exes = lib.collect lib.isDerivation project.exes;
             in ''
               # Create an intermediate dir
               mkdir release

               # Copy exes to intermediate dir
               ${lib.concatMapStringsSep
                   "\n"
                   (exe: "cp --verbose --remove-destination --update=none ${exe}/bin/* release")
                   exes}
             '';

           mkDistMusl =
             let
               project = haskellProject.projectCross.musl64;
               name = "gibberish-${version}-x86_64-linux";
               version = project.exes.gibber.identifier.version;
             in
               pkgs.runCommand
                 "gibberish-musl64"
                 {}
                 ''
                   mkdir -p $out

                   # Copy exes to intermediate dir
                   ${cpExesCmd project}

                   # Package distribution
                   cd release
                   dist_file=${name}.tar.gz
                   tar -cvzf $out/$dist_file .
                 '';

           mkDistWin64 =
             let
               inherit (pkgs) lib;
               project = haskellProject.projectCross.mingwW64;
               name = "gibberish-${version}-x86_64-windows";
               version = project.exes.gibber.identifier.version;
               env = {
                 nativeBuildInputs = [pkgs.zip];
               };
             in
               pkgs.runCommand
                 "gibberish-win64"
                 env
                 ''
                   mkdir -p $out

                   # Copy exes to intermediate dir
                   ${cpExesCmd project}

                   # Package distribution
                   cd release
                   dist_file=${name}.zip
                   find . -type f | xargs zip $out/$dist_file
                 '';

          in {
            inherit (haskellFlake) devShells;

            # Static analysis as read-only checks (gated by `nix flake check`),
            # deliberately kept out of treefmt so `nix fmt` only ever formats.
            checks = haskellFlake.checks // {
              statix = mkLintCheck "statix" [ pkgs.statix ] "statix check";
              deadnix = mkLintCheck "deadnix" [ pkgs.deadnix ] "deadnix --fail .";
              hlint = mkLintCheck "hlint" [ hlint ] "hlint .";
            };

            packages = haskellFlake.packages // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
              dist-musl = mkDistMusl;
              dist-win64 = mkDistWin64;
            };

            # Pure formatters only (`nix fmt`). Static analysis lives in checks.
            treefmt = {
              projectRootFile = "flake.nix";

              programs = {
                # Haskell — fourmolu picks up ./fourmolu.yaml automatically.
                fourmolu.enable = true;
                cabal-gild.enable = true; # *.cabal + cabal.project

                # Nix
                alejandra.enable = true;

                # Markdown
                mdformat = {
                  enable = true;
                  settings.wrap = "keep"; # don't re-wrap prose
                  plugins = ps: [ ps.mdformat-gfm ]; # GitHub-flavored markdown
                };
              };
            };
          };
      };
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
