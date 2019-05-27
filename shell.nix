{ nixpkgs ? import <nixpkgs> {} } :

let
  inherit (nixpkgs) pkgs;
  #ocamlPackages = pkgs.ocamlPackages_latest;
  ocamlPackages = pkgs.ocamlPackages;
in

pkgs.stdenv.mkDerivation {
  name = "my-ocaml-env-0";
  buildInputs = [
    ocamlPackages.ocaml
    ocamlPackages.findlib
    ocamlPackages.ppx_import
    ocamlPackages.ppx_deriving
    ocamlPackages.merlin
    ocamlPackages.menhir
    ocamlPackages.dune
    ocamlPackages.utop
    pkgs.rlwrap
    pkgs.vscode
    (pkgs.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      pkgs.dune
    ])))
  ];
}
