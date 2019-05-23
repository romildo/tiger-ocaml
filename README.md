# tiger-compiler-ocaml

A compiler for the Tiger programming language targetting LLVM and implemented in OCaml

## Preparing the environment for the project in Ubuntu >= 18.04

```
$ sudo add-apt-repository ppa:avsm/ppa
$ sudo apt update
$ sudo apt install m4
$ sudo apt install rlwrap
$ sudo apt install opam
$ opam switch create bcc328 4.07.1
$ eval $(opam env)
$ opam install dune ppx_import ppx_deriving menhir
```

## How to clean

```
$ dune clean
```

## How to compile

```
$ dune build src/driver.exe
```

## How to test the compiler

```
$ dune exec src/driver.exe
```

## References

[Recipes for OCamlLex](https://medium.com/@huund/recipes-for-ocamllex-bb4efa0afe53)

