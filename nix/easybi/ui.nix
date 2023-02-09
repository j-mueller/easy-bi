{ pkgs
, dist
, system
}: derivation {
    src = dist;
    inherit system;
    name = "easy-bi-ui";
    builder = "${pkgs.bash}/bin/bash";
    args = [./copy-ui.sh];
}