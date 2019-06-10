(* option.mli *)

val filename : unit -> string
val channel : unit -> in_channel
val print_lex : unit -> bool
val print_parse : unit -> bool

val parse_cmdline : unit -> unit
