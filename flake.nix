{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
            (final: prev: {
              # This overlay adds our project to pkgs
              easyBiProject =
                final.haskell-nix.project' {
                  src = ./.;
                  compiler-nix-name = "ghc925";
                  # This is used by `nix develop .` to open a shell for use with
                  # `cabal`
                  shell.tools = {
                    cabal = {};
                  };
                  # Non-Haskell shell tools go here
                  shell.buildInputs = with pkgs; [
                    nixpkgs-fmt
                  ];
                  # This adds `js-unknown-ghcjs-cabal` to the shell.
                  # shell.crossPlatforms = p: [p.ghcjs];
                };
            })
      ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.easyBiProject.flake {
          # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
          # crossPlatforms = p: [p.ghcjs];
        };
        in flake // {
          # Built by `nix build .`
          packages.default = flake.packages."easy-bi-cli:exe:easy-bi";
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