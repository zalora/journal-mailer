{ pkgs ? import <nixpkgs> {},
  src ? ./. } :
{
    build = pkgs.haskellPackages.buildLocalCabal src "journal-mailer";
}
