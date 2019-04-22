{ nixpkgs ? import <nixpkgs> {} } :

let
  inherit (nixpkgs) pkgs;
  ocamlPackages = pkgs.ocamlPackages_latest;
in

pkgs.stdenv.mkDerivation {
  name = "my-ocaml-env-0";
  buildInputs = [
    ocamlPackages.ocaml
    ocamlPackages.merlin
    ocamlPackages.dune
    pkgs.emacs
    pkgs.vscode
  ];
}
