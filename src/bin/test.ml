type t = { x: int }
let print_t ppf a =  Format.fprintf ppf "x is %i" a.x
let s = Format.asprintf "> %a" print_t {x = 9}


exception Error of string

let error () fmt =
    Printf.kprintf (fun msg -> 
        raise (Error ("lin? col?" ^ " " ^ msg))) fmt

let _ = error ()
