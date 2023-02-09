{ pkgs
, dist
, system
}: derivation {
    inherit system dist;
    name = "easy-bi-ui";
    coreutils = pkgs.coreutils;
    builder = "${pkgs.bash}/bin/bash";
    args = [./copy-ui.sh];
}