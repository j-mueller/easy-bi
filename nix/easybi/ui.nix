{ pkgs }:
  pkgs.buildNpmPackage {
    pname = "easy-bi-ui";
    version = "1.0.0";
    src = ../../src/ui;
    # npmDepsHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    npmDepsHash = "sha256-IdWomP7N2ZTXFo2PwvsDkibdzWtZv5FDg+WZ6dR2cLA=";
    npmBuildScript = "build";
    # We just need the build output (from dist)
    installPhase = ''
      mkdir $out
      cp -r dist/* $out
    '';
  }  