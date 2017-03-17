type t = Constraint.t list

let make (cl:Constraint.t list) : t = cl

let contains cl p = List.for_all (fun e -> Constraint.contains e p) cl

let translate dx dy cl = List.map (fun e -> Constraint.translate dx dy e) cl

(* check if c is redundant according to the constraint list c_l*)
let redundant c c_l =
  let line = Constraint.get_border c in
  let update (f1,f2) next =
    try
      let p = Line.intersection line (Constraint.get_border next) in
      match f1,f2 with
      | None,None -> None,Some(p,next)
      | Some (old_p,old_c), None | None,Some (old_p,old_c) ->
         (match Constraint.contains old_c p, Constraint.contains next old_p with
          |true,  true  -> Some(old_p,old_c), Some(p,next)
          |true,  false -> Some(p,next),None
          |false, true  -> Some(old_p,old_c),None
          |false, false -> raise Exit
         )
      | Some (a), Some (b) ->
         match List.filter (fun (p,c) -> Constraint.contains next p) [a;b] with
         | [] -> raise Exit
         | [x] -> Some x, Some (p,next)
         | _ -> Some a, Some b
    with Line.(Error (Parallel _)) ->
      if Constraint.contains next (Line.arbitrary_point line) then
        f1,f2
      else raise Exit
  in
  try List.fold_left update (None,None) c_l |> ignore; false
  with Exit -> true

let remove_redundancies poly =
  let rec aux acc rest =
    match rest with
    | [] -> acc
    | h::t ->
       if redundant h (List.rev_append acc t) then aux acc t
       else aux (h::acc) t
  in aux [] poly

let intersection p1 p2 = List.rev_append p1 p2

let of_polygon p =
  let b = Point.barycenter (List.map (fun p->p,1.) (Polygon.to_list p)) in
  let mk_constraint p1 p2 =
    let line = Line.of_points p1 p2 in
    let c1 = Constraint.(make line Geq) in
    if Constraint.contains c1 b then c1 else
      Constraint.(make line Leq)
  in
  Polygon.fold (fun acc p1 p2 ->
      try (mk_constraint p1 p2)::acc
      with Line.Error _ -> acc
    ) [] p

let get_constr p = p
