{ nixpkgs ? import <nixpkgs> {} } :

let
  inherit (nixpkgs) pkgs;
  ocamlPackages = pkgs.ocamlPackages_latest;
in

pkgs.stdenv.mkDerivation {
  name = "my-ocaml-env-0";
  buildInputs = [
    ocamlPackages.ocaml
    ocamlPackages.findlib
    ocamlPackages.ppx_import
    ocamlPackages.ppx_deriving
    ocamlPackages.merlin
    ocamlPackages.dune
    ocamlPackages.utop
    pkgs.emacs
    pkgs.vscode
    pkgs.rlwrap
  ];
}
