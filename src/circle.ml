open Math

module Make
         (A:Arith.T)
         (P:Signatures.Point_Sig with type arith = A.t)
         (L:Signatures.Line_sig with type point = P.t and type arith = A.t)
         (S:Signatures.Segment_sig with type point = P.t and type line = L.t)
  = struct

  type arith = A.t
  type segment = S.t
  type point = P.t
  type line = L.t

  type t = {center:P.t; radius:arith}

  open A

  let make center radius =
    if radius >= zero then {center;radius}
    else invalid_arg "Circle.make:radius should be positive or zero"

  let center {center;_} = center

  let radius {radius;_} = radius

  let translate dx dy c =
    {c with center = P.translate dx dy c.center}

  let point_reflection p c =
    {c with center =P.point_reflection p c.center}

  let rotate c p f =
    {c with center = P.rotate c.center p f}

  let rotate_angle c p f =
    {c with center = P.rotate_angle c.center p f}

  let contains c p =
    P.sq_distance c.center p <= (c.radius*.c.radius)

  let area {radius;_} = (A.of_float pi) *. radius *. radius

  let perimeter {radius;_} = two *. (A.of_float pi) *. radius

  let proj_x c = (c.center.x-.c.radius,c.center.x+.c.radius)

  let proj_y c = (c.center.y-.c.radius,c.center.y+.c.radius)

  let intersects c1 c2 =
    (P.sq_distance c1.center c2.center) < let d = (c1.radius +. c2.radius) in d *. d

  (** line_intersection takes a circle and line and returns the list of the
    intersection points. (can be [], [a] or [a,b] *)
  let intersect_line c (l:L.t) =
    let cx = c.center.P.x and cy = c.center.P.y in
    let open L in
    match l with
    | X(x) ->
       let a = x-.cx in
       Math.solve 1. 0. (A.to_float (a*.a -. c.radius*.c.radius)) |>
         List.map (fun y -> P.make x (y |> A.of_float) |> P.translate zero cy)
    | _ ->
       (* we go to origin *)
       let l_2 = translate (neg cx) (neg cy) l in
       let a,b = (match l_2 with Y(a,b) -> a,b | _ -> assert false) in
       (* we solve the equation at the origin for x*)
       (* x² + y² = r²   and   y = ax+b
       => x² + (ax + b)² = r²
       => x² + a²x² + 2abx + b² = r²
       => (a²+1)x² + 2abx + b²-r² = 0 *)
       Math.solve (A.to_float (a*.a+.one)) (A.to_float (two*.a*.b))
         (A.to_float (b*.b -. c.radius*.c.radius))
       (* we calculate the associated y*)
       (* and translate back the result to the first coordinates*)
       |> List.map (fun x -> P.make (A.of_float x) (a*.(A.of_float x)+.b) |> P.translate cx cy)

  let segment_intersection c (s:S.t) =
    S.to_line s |> intersect_line c |> List.filter (S.contains s)

  (** tangent c p returns the tangent of circle c going through point p.
    p must lie on c's boundary *)
  let tangent {center;_} p =
    L.perpendicular_of_line (L.of_points center p) p

  let intersection c c' =
    let c1_c2 = L.of_points c.center c'.center in
    let p = P.barycenter [(c.center,c.radius);(c'.center,c'.radius)] in
    let l = L.perpendicular_of_line c1_c2 p in
    intersect_line c l

  let circumscribed p1 p2 p3 =
    let b1 = L.point_bissection p1 p2
    and b2 = L.point_bissection p2 p3 in
    let center = L.intersection b1 b2 in
    let radius = (P.distance p2 center) in
    make center radius

  let incircle p1 p2 p3 =
    let a = P.distance p1 p2
    and b = P.distance p2 p3
    and c = P.distance p3 p1 in
    let center = P.barycenter [(p1,b);(p2,c);(p3,a)] in
    let radius =
      let s = (a+.b+.c) /. two  in
      let sq = s *. (s-.a) *. (s-.b) *.(s-.c) |> A.to_float |> sqrt |> A.of_float in
      two *. sq /. (a+.b+.c)
    in make center radius

  let of_diameter p1 p2 =
    let c = P.center p1 p2 in
    let radius = (P.distance p1 c) in
    make c radius

  let bounding (pts : P.t list) =
    let of_two x y pt =
      try
        [((x,pt),y);((y,pt),x)]
        |> List.map (fun ((a,b),c) -> (([a;b],of_diameter a b),c))
        |> List.find (fun ((_,circle),inner) -> contains circle inner)
        |> fst
      with Not_found -> ([x;y;pt],(circumscribed x y pt))
    in
    let of_three x y z pt =
      let found =
        [((x,y),z); ((x,z),y); ((y,z),x)]
        |> List.map (fun ((a,b),c) -> ((of_two a b pt),c))
        |> List.filter (fun ((_,c),inner) -> contains c inner)
        |> List.map fst
      in
      List.fold_left (fun (e1,c1) (e2,c2) ->
          if radius c1 < radius c2 then (e1,c1) else (e2,c2)
        ) (List.hd found) (List.tl found)
    in
    let update set pt =
      match set with
      | [x] -> [x;pt],(of_diameter x pt)
      | [x;y] -> of_two x y pt
      | [x;y;z] -> of_three x y z pt
      | _ -> assert false
    in
    let rec mindisk l circle set pts =
      match pts with
      | [] -> circle
      | h::tl when contains circle h || List.mem h set ->
         mindisk l circle set tl
      | h::_ ->
         let (new_set,new_circle) = update set h in
         List.filter (fun e -> List.mem e new_set |> not) l |>
           mindisk l new_circle new_set
    in
    match pts with
    | [] -> invalid_arg "can't build a bounding circle with an empty list"
    | h::tl -> mindisk pts (make h zero) [h] tl

  let random_angle r =
    let theta = (Random.float pi |> A.of_float) *. two in
    let x = r *. (cos (A.to_float theta) |> A.of_float)
    and y = r *. (sin (A.to_float theta) |> A.of_float)
    in x,y

  let random_point c : P.t =
    let r = (Random.float (c.radius *. c.radius |> A.to_float)) |> sqrt |> A.of_float in
    let x,y = random_angle r in
    P.(make (c.center.x+.x) (c.center.y +. y))

  let random_point_perimeter c : P.t =
    let r = c.radius  in
    let x,y = random_angle r in
    P.(make (c.center.x+.x) (c.center.y +. y))

  let print fmt c =
    Format.fprintf fmt "center:%a, radius=%a" P.print c.center A.pp_print c.radius
end
