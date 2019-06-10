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
    ocamlPackages.ppx_expect
    ocamlPackages.ppx_here
    ocamlPackages.re
    ocamlPackages.merlin
    ocamlPackages.patience_diff
    ocamlPackages.menhir
    ocamlPackages.ounit
    ocamlPackages.dune
    ocamlPackages.utop
    pkgs.rlwrap
    pkgs.vscode
    (pkgs.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      pkgs.dune
    ])))
  ];
}
