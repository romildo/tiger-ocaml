(* option.ml *)

let file = ref ""
let inch = ref stdin
let lex  = ref false

let filename ()  = !file
let channel ()   = !inch
let print_lex ()   = !lex

let set_input s =
  try
    file := s;
    inch := open_in s
  with Sys_error err ->
    raise (Arg.Bad ("Could not open file " ^ err))

let usage_msg =
  "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... FILE\n"

let rec usage () =
  Arg.usage options usage_msg;
  exit 0

and options =
  [ "-lex",   Arg.Set lex,    "\tDisplay sequence of lexical symbols"
  ; "-help",  Arg.Unit usage, "\tDisplay this list of options"
  ; "--help", Arg.Unit usage, "\tDisplay this list of options"
  ]

let parse_cmdline () =
  Arg.parse options set_input usage_msg
