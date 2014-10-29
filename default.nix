{ pkgs ? import <nixpkgs> {},
  src ? builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist") ./.
} :
{
    build = pkgs.haskellPackages.buildLocalCabal src "journal-mailer";
}
