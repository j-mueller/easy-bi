{ compiler ? "ghc925"
, system ? builtins.currentSystem
, haskellNix
, nixpkgs
}:
let
    pkgs = import nixpkgs {
        inherit system;
        overlays = [
        haskellNix.overlay
        ];
    };

    
  hsPkgs = pkgs.haskell-nix.project {
    # TODO: probably should use flake.nix inputs.self here
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hydra";
      src = ./../..;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

  };

in {
    inherit compiler pkgs hsPkgs;
}
