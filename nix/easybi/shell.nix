{ withoutDevTools ? false
, easyBiProject
, system ? builtins.currentSystem
}:
let
    inherit (easyBiProject) compiler pkgs hsPkgs;

    libs = [];

    buildInputs = [
        pkgs.git
        pkgs.pkg-config
        pkgs.haskellPackages.hspec-discover
        pkgs.haskellPackages.cabal-plan
        pkgs.nodejs
        pkgs.nodePackages.npm
    ];

    
    devInputs = if withoutDevTools then [ ] else [
        # The interactive Glasgow Haskell Compiler as a Daemon
        pkgs.haskellPackages.ghcid
        pkgs.sqlite
    ];

    # Haskell.nix managed tools (via hackage)
    buildTools = {
        cabal = "3.8.1.0";
    };

    devTools = if withoutDevTools then { } else {
        # fourmolu = "0.4.0.0"; # 0.5.0.0 requires Cabal 3.6
        # haskell-language-server = "latest";
    };


    haskellNixShell = hsPkgs.shellFor {
        # NOTE: Explicit list of local packages as hoogle would not work otherwise.
        # Make sure these are consistent with the packages in cabal.project.
        packages = ps: with ps; [
            easy-bi-cli
            easy-bi-vis
            easy-bi-sql
        ];

        tools = buildTools // devTools;

        buildInputs = libs ++ buildInputs ++ devInputs;

        withHoogle = !withoutDevTools;

        # Always create missing golden files
        CREATE_MISSING_GOLDEN = 1;

        # Force a UTF-8 locale because many Haskell programs and tests
        # assume this.
        LANG = "en_US.UTF-8";

        GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    };

in {
    default = haskellNixShell;
}