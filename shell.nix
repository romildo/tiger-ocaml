{ nixpkgs ? import <nixpkgs> {} } :

let
  inherit (nixpkgs) pkgs;
  ocamlPackages = pkgs.ocamlPackages_latest;
in

pkgs.stdenv.mkDerivation {
  name = "my-ocaml-env-0";
  buildInputs = [
    ocamlPackages.dune
    ocamlPackages.findlib
    ocamlPackages.menhir
    ocamlPackages.merlin
    ocamlPackages.ocaml
    ocamlPackages.ounit
    ocamlPackages.patience_diff
    ocamlPackages.ppx_deriving
    ocamlPackages.ppx_expect
    ocamlPackages.ppx_here
    ocamlPackages.ppx_import
    ocamlPackages.re
    ocamlPackages.utop
    pkgs.ocamlformat
    pkgs.rlwrap
    pkgs.vscode
    (pkgs.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      pkgs.dune
      pkgs.ocamlformat
    ])))
  ];
}
