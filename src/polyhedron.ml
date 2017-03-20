(* TODO : change representation to a sorted list? *)

type t = Constraint.t list

(* subset of a line that satisfies a set of constraint *)
(* None,None -> all the line *)
(* Some (p,c),_ -> the subpart which satisfies the constraint c.
                 p is the intersection point *)
type subline = (Point.t * Constraint.t option) * (Point.t * Constraint.t option)

exception Emptyset

(* returns the subpart of l that satisfies cstrs.
   raises Emptyset if no part l satisfies cstrs *)
let subline l cstrs =
  let update (f1,f2) next =
    try
      let p = Line.intersection l (Constraint.get_border next) in
      match f1,f2 with
      | None,None -> None,Some(p,next)
      | Some (old_p,old_c), None | None,Some (old_p,old_c) ->
         (match Constraint.contains old_c p, Constraint.contains next old_p with
          |true,  true  -> Some(old_p,old_c), Some(p,next)
          |true,  false -> Some(p,next),None
          |false, true  -> Some(old_p,old_c),None
          |false, false -> raise Emptyset
         )
      | Some (a), Some (b) ->
         match List.filter (fun (p,c) -> Constraint.contains next p) [a;b] with
         | [] -> raise Emptyset
         | [x] -> Some x, Some (p,next)
         | _ -> Some a, Some b
    with Line.(Error (Parallel _)) ->
      if Constraint.contains next (Line.arbitrary_point l) then f1,f2
      else raise Emptyset
  in
  List.fold_left update (None,None) cstrs

let fold_pairs f acc =
  let rec loop past acc = function
  | [] -> acc
  | h::t -> loop (h::past) (f acc h (List.rev_append past t)) t
  in loop [] acc

let is_closed c_l =
  try fold_pairs (fun acc e rest ->
          let line = Constraint.get_border e in
          (match subline line rest with
           | None,_ | _,None -> raise Exit
           | _ -> acc
           | exception Emptyset -> acc)
        ) true c_l
  with Exit -> false

let is_open c_l = is_closed c_l |> not

(* check if c is redundant according to the constraint list c_l*)
let redundant c c_l =
  let line = Constraint.get_border c in
  try ignore (subline line c_l); false
  with Emptyset -> true

let is_empty = function
  | [] -> false
  | x -> let c = Constraint.(make Line.x_axis Constraint.Gt) in
         redundant c x && redundant (Constraint.complementary c) x

let remove_redundancies =
  fold_pairs (fun acc e rest -> if redundant e rest then acc else (e::acc)) []

let make (cl:Constraint.t list) : t = cl

let contains cl p = List.for_all (fun e -> Constraint.contains e p) cl

let translate dx dy cl = List.rev_map (fun e -> Constraint.translate dx dy e) cl

let intersection p1 p2 = List.rev_append p1 p2

let of_polygon p =
  let exception LT3 of Point.t list in
  try
    let arbitrary =
      match Polygon.to_list p with
      | h1::h2::h3::_ -> Point.center h1 h2 |> Point.center h3
      | x -> raise (LT3 x)
    in
    let mk_constraint p1 p2 =
      let line = Line.of_points p1 p2 in
      let c1 = Constraint.(make line Geq) in
      if Constraint.contains c1 arbitrary then c1 else
        Constraint.(make line Leq)
    in
    Polygon.fold (fun acc p1 p2 ->
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
          let line = Constraint.get_border e in
          (match subline line rest with
           | None,_ | _,None -> raise Exit
           | Some (p1,_), Some (p2,_) -> p1::p2::acc
           | exception Emptyset -> acc)
        ) [] cl
      |> Polygon.bounding
  with Exit -> failwith "can't convert an open polyhedron to a polygon"

let get_constr p = p
