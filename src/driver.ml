(* driverl.ml *)

let _ =
  Printexc.record_backtrace true;
  Option.parse_cmdline ();
  print_endline "done"
