(* tree.ml *)

type 'a tree = Tree of 'a * 'a tree list
[@@deriving show]

let mkt x ts = Tree (x, ts)


let rec map f (Tree (info, children)) =
  Tree (f info, List.map (map f) children)


let string_of_tree t =
  let buf = Buffer.create 16 in
  let rec to_string indent =
    let child_to_string continue_indentation mark x =
      Buffer.add_char buf  '\n';
      Buffer.add_string buf indent;
      Buffer.add_string buf mark;
      to_string (indent ^ continue_indentation) x
    in
    let rec children_to_string = function
      | [x]     -> child_to_string "   " "└─ " x
      | x :: xs -> child_to_string "│  " "├─ " x;
                   children_to_string xs
      | []      -> ()
    in
    function
      | Tree (x,children) -> Buffer.add_string buf x;
                             children_to_string children
  in
  to_string "" t;
  Buffer.contents buf


let asy_of_tree t =
  let buf = Buffer.create 256 in
  let rec go root k (Tree (x,ts)) =
    let name = if root = "" then "node" else root ^ "_" ^ string_of_int k in
    Buffer.add_string buf
      (Printf.sprintf "TreeNode %s = nodeGM(%s\"%s\"%s);\n"
         name
         (if root = "" then "" else root ^ ", ")
         x
         "");
    ignore (List.fold_left (fun n t -> go name n t; n+1) 1 ts)
  in
  go "" 0 t;
  Printf.sprintf
    "import drawtree;
     treeLevelStep = 30mm;
     treeNodeStep = 2mm;
     TreeNode nodeGM( TreeNode parent = null, Label label, pen coul1=1bp+gray, pen coul2=paleblue )
     {
       frame f;
       roundbox( f, label, xmargin=1.5mm, filltype=FillDraw(fillpen=coul2,drawpen=coul1) );
       return makeNode( parent, f );
     }
     %s
     draw(node, (0,0));
    "
    (Buffer.contents buf)


let dot_of_tree name t =
  let buf = Buffer.create 256 in
  let rec go name (Tree (x,ts)) =
    Buffer.add_string buf (Printf.sprintf "%s [label=\"%s\"];\n" name x);
    ignore (List.fold_left
              (fun n t ->
                let name' = name ^ "_" ^ string_of_int n in
                go name' t;
                Buffer.add_string buf (Printf.sprintf "%s -> %s [arrowhead=none];\n" name name');
                n+1)
              1
              ts)
  in
  go "n" t;
  Printf.sprintf
    "digraph %s {
node [shape=record];
%s
}
"
    name
    (Buffer.contents buf)


let str_replace c1 c2 str =
  try
    let i = String.index str c1 in
    let s = Bytes.of_string str in
    Bytes.set s i c2;
    Bytes.to_string s
  with Not_found -> str

let box_node x =
  Box.frame (Box.line (str_replace ':' '\n' x))

let rec box_of_tree = function
  | Tree (x,[]) -> box_node x
  | Tree (x,ts) -> Box.connect (box_node x) (List.map box_of_tree ts)
