{
  perSystem = {
    # Pure formatters only (`nix fmt`). Static analysis lives in checks.
    treefmt = {
      projectRootFile = "flake.nix";

      programs = {
        alejandra.enable = true; # Nix
        just.enable = true; # Justfile

        # Haskell
        fourmolu.enable = true;
        cabal-gild.enable = true;

        # Markdown
        mdformat = {
          enable = true;
          settings.wrap = "keep";
          plugins = ps: [ps.mdformat-gfm];
        };
      };
    };
  };
}
