{ compiler ? "ghc947"
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
      name = "easy-bi";
      src = ./../..;
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    modules = [
      {
          packages.easy-bi-cli.dontStrip = false;
      }
    ];

  };

in {
    inherit compiler pkgs hsPkgs;
}
