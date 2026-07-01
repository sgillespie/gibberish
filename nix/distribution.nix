{inputs, ...}: {
  perSystem = {
    system,
    config,
    haskellProject,
    lib,
    pkgs,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) isx86_64 isLinux;

    cpExesCmd = project: let
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

    mkDistMusl = let
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

    mkDistWin64 = let
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
    packages = lib.optionalAttrs (system == "x86_64-linux") {
      x86_64-linux-static-dist = mkDistMusl;
      x86_64-windows-dist = mkDistWin64;
    };
  };
}
