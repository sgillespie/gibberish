{
  perSystem = {
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
          plugins = ps: [ps.mdformat-gfm]; # GitHub-flavored markdown
        };
      };
    };
  };
}
