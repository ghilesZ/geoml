(* FIXME : subline + redundancy check *)

type t = Constraint.t list

let print fmt plh =
  List.iter (Format.fprintf fmt "%a\n" Constraint.print) plh

(* subset of a line that satisfies a set of constraint *)
(* None,None -> all the line *)
(* Some (p,c),_ -> the subpart which satisfies the constraint c.
                 p is the intersection point *)
type subline = (Point.t * Constraint.t) option * (Point.t * Constraint.t) option

let normalize ((f1,f2):subline) =
  match f1,f2 with
  | None,None -> None,None
  | x, None | None, x -> x,None
  | Some (p1,c1),Some (p2,c2) ->
     (* we try to keep a normalized order *)
     if (p1,c1) < (p2,c2) then Some(p1,c1), Some(p2,c2)
     else Some(p2,c2), Some(p1,c1)

exception Emptyset

let reduce_subline l (f1,f2) constr =
    try
      let p = Line.intersection l (Constraint.get_border constr) in
      match f1,f2 with
      | None,None -> Some(p,constr),None
      | Some (old_p,old_c), None ->
         (match Constraint.contains old_c p, Constraint.contains constr old_p with
          |true,  true  -> Some(p,constr),Some(old_p,old_c)
          |true,  false -> Some(p,constr),None
          |false, true  -> Some(old_p,old_c),None
          |false, false -> raise Emptyset)
      | None,Some (_,_) -> assert false
      | Some (pa,ca), Some (pb,cb) ->
         match Constraint.contains constr pa, Constraint.contains constr pb with
         | false,false -> raise Emptyset
         | true,false -> Some (pa,ca), Some (p,constr)
         | false,true -> Some (p,constr), Some (pb,cb)
         | _ -> f1,f2
    with Line.(Error (Parallel (_,_))) ->
      if Constraint.contains constr (Line.arbitrary_point l) then f1,f2
      else raise Emptyset

let subline (l:Line.t) (cstrs:Constraint.t list) : subline =
  List.fold_left (reduce_subline l) (None,None) cstrs

let fold_pairs f acc =
  let rec loop past acc = function
    | [] -> acc
    | h::t -> loop (h::past) (f acc h (List.rev_append past t)) t
  in loop [] acc

let is_closed (c_l:Constraint.t list) =
  try fold_pairs (fun acc e rest ->
          let l = Constraint.get_border e in
          (match subline l rest with
           | None,_ | _,None -> raise Exit
           | _ -> acc
           | exception Emptyset -> acc)
        ) true c_l
  with Exit -> false

let is_open c_l = is_closed c_l |> not

(* check if c is redundant according to the constraint list c_l *)
let redundant c c_l =
  try fold_pairs (fun acc e rest ->
          let l = Constraint.get_border e in
          (match subline l rest with
           | x ->
              (try
                 let with_new_constraint = subline l (c::rest) in
                 if (normalize x) = normalize with_new_constraint then acc
                 else raise Exit
               with Emptyset -> raise Exit)
           | exception Emptyset -> acc)
        ) true c_l
  with Exit -> false

let remove_redundancies cstrs =
  let ( @ ) = List.rev_append in
  let rec loop acc = function
    | [] -> acc
    | h::t ->
       if redundant h (acc@t) then loop acc t
       else loop (h::acc) t
  in loop [] cstrs

let is_empty = function
  | [] | [_]-> false
  | l ->
     let x_neg = Constraint.(make (Line.make_y 1. 0.) Constraint.Gt)
     and x_pos = Constraint.(make (Line.make_y 1. 0.) Constraint.Leq) in
     let res = redundant x_neg l && redundant x_pos l in
     res

let make (cl:Constraint.t list) : t = cl

let contains cl p = List.for_all (fun e -> Constraint.contains e p) cl

let translate dx dy cl = List.rev_map (fun e -> Constraint.translate dx dy e) cl

let intersection p1 p2 = List.rev_append p1 p2

let of_polygon p =
  let exception LT3 of Point.t list in
  try
    let arbitrary =
      match Polygon.Convex.to_list p with
      | h1::h2::h3::_ -> Point.center h1 h2 |> Point.center h3
      | x -> raise (LT3 x)
    in
    let mk_constraint p1 p2 =
      let line = Line.of_points p1 p2 in
      let c1 = Constraint.(make line Geq) in
      if Constraint.contains c1 arbitrary then c1
      else
        let c2 = Constraint.(make line Leq) in
        if Constraint.contains c2 arbitrary then c2
        else failwith "shoud not occur"
    in
    Polygon.Convex.fold (fun acc p1 p2 ->
        try (mk_constraint p1 p2)::acc
        with Line.Error _ -> acc
      ) [] p
  with LT3 pts ->
    match pts with
    | [p1;p2] -> let line = Line.of_points p1 p2 in
                 make [Constraint.(make line Geq);Constraint.(make line Leq)]
    | [p] -> let x = Line.make_x p.Point.x and y = Line.make_y 0. p.Point.y in
             make [Constraint.(make x Geq); Constraint.(make y Geq);
                   Constraint.(make x Leq); Constraint.(make y Leq)]
    | _ -> failwith "can't build polyhedron with no point"

let to_polygon cl =
  try fold_pairs (fun acc e rest ->
          let l = Constraint.get_border e in
          (match subline l rest with
           | None,_ | _,None -> raise Exit
           | Some (p1,_), Some (p2,_) -> p1::p2::acc
           | exception _ -> acc
          )
        ) [] cl
      |> Polygon.Convex.hull
  with Exit -> failwith "can't convert an open polyhedron to a polygon"

let get_constr p = p
