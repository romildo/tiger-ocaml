(* box.ml *)

module U = CamomileLibraryDefault.Camomile.UTF8
module UC = CamomileLibraryDefault.Camomile.UChar

let value option default =
  match option with
  | None -> default
  | Some x -> x

let str_length = U.length

let str_split c s =
  let uc = UC.of_char c in
  let rec go i =
    if U.out_of_range s i then
      []
    else
      let rec go2 j =
        if U.out_of_range s j then
          [ String.sub s i (j - i) ]
        else if U.look s j = uc then
          String.sub s i (j - i) :: go (U.next s j)
        else
          go2 (U.next s j)
      in
      go2 (U.next s i)
  in
  go (U.first s)

type box = string list

let replicate n x =
  let rec go n xs =
    if n > 0
    then go (n-1) (x::xs)
    else xs
  in
  go n []

let str_make n s = String.concat "" (replicate n s)

let maximum = function
  | [] -> None
  | hd::tl -> Some (List.fold_left max hd tl)

let char x = [String.make 1 x]

(* let uniform x width height = replicate height (String.make width x) *)
let uniform x width height =
  replicate height (str_make width x)

let line x = [x]

let line x =
  if x = "" then [x]
  else
    let xs = str_split '\n' x in
    let w = value (maximum (List.map U.length xs)) 0 in
    List.map
      (fun x ->
        let n = U.length x in
        String.make ((w - n)/2) ' ' ^ x ^ String.make (w - n - (w - n)/2) ' '
      )
      xs

let lines xs = xs


let string_of_box = function
  | [] -> ""
  | x :: xs ->
    let buf = Buffer.create 16 in
    Buffer.add_string buf x;
    List.iter (fun x -> Buffer.add_char buf '\n'; Buffer.add_string buf x) xs;
    Buffer.contents buf

let width = function
  | x :: _ -> U.length x
  | [] -> 0

let height xs = List.length xs


let rec widen box new_width =
  let w = width box in
  if new_width <= w then
    box
  else
    let h = height box in
    let left = uniform " " ((new_width - w) / 2) h in
    let right = uniform " " (new_width - w - width left) h in
    beside left (beside box right)

and heighten box new_height =
  let h = height box in
  if new_height <= h then
    box
  else
    let w = width box in
    let top = uniform " " w ((new_height - h) / 2) in
    let bottom = uniform " " w (new_height - h - height top) in
    above top (above box bottom)

and heighten2 box new_height =
  let h = height box in
  if new_height <= h then
    box
  else
    above box (uniform " " (width box) (new_height - h))

and above b1 b2 =
  List.append (widen b1 (width b2)) (widen b2 (width b1))

and beside b1 b2 =
  List.map2 (^) (heighten b1 (height b2)) (heighten b2 (height b1))

and beside2 b1 b2 =
  List.map2 (^) (heighten2 b1 (height b2)) (heighten2 b2 (height b1))


let frame box =
  let horiz = uniform "─" (width box) 1
  and vert = uniform "│" 1 (height box) in
  above
    (beside
       (line "┌")
       (beside horiz (line "┐")))
    (above
       (beside
          vert
          (beside box vert))
       (beside
          (line "└")
          (beside horiz (line "┘"))))


let connect box = function
  | []  -> box
  | [b] -> above box (above (line "|") b)
  | b1::bs  ->
    let rec go = function
      | [] -> invalid_arg "go inside connect"
      | [b] -> let w = width b in
               str_make (w/2) "─" ^ "┐" ^ String.make (w-w/2+1) ' '
      | b::bs -> let w = width b in
                 str_make (w/2) "─" ^ "┬" ^ str_make (w-w/2+1) "─" ^ go bs
    in
    let w = width b1 in
    let buf = String.make (w/2-1) ' ' ^ "┌" ^ str_make (w-w/2) "─" ^ go bs in
    let q = (U.length buf - 1)/2 in
    let q1 = U.nth buf q in
    let q2 = U.next buf q1 in
    let buf = String.sub buf 0 q1 ^ "┴" ^ String.sub buf q2 (String.length buf - q2) in
    (* buf.[q] <- if buf.[q] = '-' then '+' else '+'; *)
    let box2 = List.fold_left (fun b1 b2 -> beside2 b1 (beside2 (char ' ') b2)) b1 bs in
    above box (above (line buf) box2)
