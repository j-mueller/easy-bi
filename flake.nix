{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        easyBiProject = import ./nix/easybi/project.nix { inherit system haskellNix nixpkgs; };
        easyBiPackages = import ./nix/easybi/packages.nix {
          inherit system easyBiProject;
        };
        easyBiImages = import ./nix/easybi/docker.nix {
          inherit easyBiPackages system nixpkgs;
        };
        in {
          devShells = (import ./nix/easybi/shell.nix {
            inherit easyBiProject system;
          }) // {
            ci = (import ./nix/easybi/shell.nix {
              inherit easyBiProject system;
              withoutDevTools = true;
              }).default;
          };
          packages = easyBiPackages // {
            docker = easyBiImages;
          };
          nixConfig = {
            extra-substituters = [
              "https://cache.iog.io"
            ];
            extra-trusted-public-keys = [
              "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
            ];
          };
          allow-import-from-derivation = true;
        });
}