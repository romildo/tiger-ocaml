exception Error of (Location.t * string)

let () =
  Printexc.register_printer
    (function
      | Error (loc, msg) -> Some (Format.asprintf "%a error: %s" Location.pp_location loc msg)
      | _ -> None (* for other exceptions *)
    )

let error loc fmt =
  Format.ksprintf (fun msg -> raise (Error (loc, msg))) fmt

let fatal fmt =
  Format.ksprintf failwith fmt
