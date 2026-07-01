{inputs, ...}: {
  imports = [
    {
      perSystem = {system, ...}: {
        # Inject haskell.nix overlay
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [inputs.haskellNix.overlay];
        };
      };
    }
  ];

  perSystem = {
    system,
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) isx86_64 isLinux;

    crossPlatforms = p:
      lib.optionals isx86_64 [p.mingwW64]
      ++ lib.optionals (isx86_64 && isLinux) [p.musl64];

    cabalProject = pkgs.haskell-nix.cabalProject' {
      src = ./..;
      compiler-nix-name = "ghc910";
      name = "gibberish";

      flake.variants.profiled = {
        modules = [
          {
            enableLibraryProfiling = true;
            enableProfiling = true;
          }
        ];
      };

      shell = {
        tools = {
          cabal = "latest";
          haskell-language-server = "latest";
          hp2pretty = "latest";
        };

        withHoogle = true;

        # We don't need cross platforms in the shell; should speed up evaluation
        crossPlatforms = _: [];
      };
    };

    # Add exes to cabal project
    haskellProject = cabalProject.appendOverlays [
      pkgs.haskell-nix.haskellLib.projectOverlays.projectComponents
    ];

    haskellFlake = haskellProject.flake {
      inherit crossPlatforms;
    };
  in {
    # expose haskellProject to other modules
    _module.args.haskellProject = haskellProject;

    inherit (haskellFlake) packages devShells checks apps;
  };
}
