{...} @ top: {
  perSystem = {pkgs, ...}: let
    # Source filtered by .gitignore, for read-only static-analysis checks.
    lintSrc = pkgs.nix-gitignore.gitignoreSource [] ./..;

    mkLintCheck = name: nativeBuildInputs: checkPhase:
      pkgs.runCommandLocal "${name}-check" {inherit nativeBuildInputs;} ''
        cd ${lintSrc}
        ${checkPhase}
        touch $out
      '';
  in {
    # Static analysis as read-only checks (gated by `nix flake check`),
    # deliberately kept out of treefmt so `nix fmt` only ever formats.
    checks = {
      statix = mkLintCheck "statix" [pkgs.statix] "statix check";
      deadnix = mkLintCheck "deadnix" [pkgs.deadnix] "deadnix --fail .";
      hlint = mkLintCheck "hlint" [pkgs.hlint] "hlint .";
    };
  };
}
