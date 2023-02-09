{ easyBiProject
, system ? builtins.currentSystem
, ui
}:
let
    nativePkgs = easyBiProject.hsPkgs;
    patchedForCrossProject = easyBiProject.hsPkgs.appendModule
    ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; });
    musl64Pkgs = patchedForCrossProject.projectCross.musl64.hsPkgs;

    cli = nativePkgs.easy-bi-cli.components.exes.easy-bi;

in
    { 
        easy-bi-cli = cli;
        easy-bi-cli-static = musl64Pkgs.easy-bi-cli.components.exes.easy-bi;

        easy-bi-ui = cli;

        # Built by `nix build .`
        default = cli;
    }