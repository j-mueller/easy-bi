{ src
, system
, pkgs
}:
derivation {
  inherit src system;
  npm    = pkgs.nodePackages.npm;
  nodejs = pkgs.nodejs;          
  bash    = pkgs.bash;      
  name    = "easy-bi-ui";
  builder = "${pkgs.bash}/bin/bash";
  args    = [./build-ui.sh];
}