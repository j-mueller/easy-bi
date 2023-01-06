{ easyBiPackages
, system ? builtins.currentSystem
, nixpkgs
}:
let
    pkgs = import nixpkgs { inherit system; };
in {
    easy-bi-server = pkgs.dockerTools.buildImage {
        name = "easy-bi-server";
        tag = "latest";
        created = "now";
        config = {
            Entrypoint = [ "${easyBiPackages.easy-bi-cli-static}/bin/easy-bi" ];
        };
    };
}