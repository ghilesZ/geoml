open Math

type t = {center:Point.t; radius:float}

let make center radius : t =
  if radius >= 0. then
    {center;radius}
  else invalid_arg "Circle.make:radius should be positive or zero"

let center {center;_} = center

let radius {radius;_} = radius

let translate dx dy (c:t) =
  {c with center =Point.translate dx dy c.center}

let point_reflection p (c:t) =
  {c with center =Point.point_reflection p c.center}

let rotate (c:t) p f =
  {c with center = Point.rotate c.center p f}

let rotate_angle (c:t) p f =
  {c with center = Point.rotate_angle c.center p f}

let contains (c:t) p =
  Point.sq_distance c.center p <= (c.radius*.c.radius)

let area ({radius;_}:t) = pi *. radius *. radius

let perimeter ({radius;_}:t) = 2. *. pi *. radius

let proj_x (c:t) = let open Point in (c.center.x-.c.radius,c.center.x+.c.radius)

let proj_y (c:t) = let open Point in (c.center.y-.c.radius,c.center.y+.c.radius)

let intersects (c1:t) (c2:t) =
  (Point.sq_distance c1.center c2.center) < (c1.radius +. c2.radius) ** 2.

(** line_intersection takes a circle and line and returns the list of the
    intersection points. (can be [], [a] or [a,b] *)
let intersect_line (c:t) (l:Line.t) =
  let cx = c.center.Point.x and cy = c.center.Point.y in
  let open Line in
  match l with
  | X(x) ->
     let a = x-.cx in
     Math.solve 1. 0. (a*.a -. c.radius*.c.radius)
     |> List.map (fun y -> Point.make x y |> Point.translate 0. cy)
  | _ ->
     (* we go to origin *)
     let l_2 = translate (-.cx) (-.cy) l in
     let a,b = (match l_2 with Y(a,b) -> a,b | _ -> assert false) in
     (* we solve the equation at the origin for x*)
     (* x² + y² = r²   and   y = ax+b
       => x² + (ax + b)² = r²
       => x² + a²x² + 2abx + b² = r²
       => (a²+1)x² + 2abx + b²-r² = 0 *)
     Math.solve (a*.a+.1.) (2.*.a*.b) (b*.b -. c.radius*.c.radius)
     (* we calculate the associated y*)
     (* and translate back the result to the first coordinates*)
     |> List.map (fun x -> Point.make x (a*.x+.b) |> Point.translate cx cy)

let segment_intersection c (s:Segment.t) =
  Segment.to_line s |> intersect_line c |> List.filter (Segment.contains s)

(** tangent c p returns the tangent of circle c going through point p.
    p must lie on c's boundary *)
let tangent {center;_} p =
  Line.perpendicular_of_line (Line.of_points center p) p

let intersection (c:t) (c':t) =
  let c1_c2 = Line.of_points c.center c'.center in
  let p = Point.barycenter [(c.center,c.radius);(c'.center,c'.radius)] in
  let l = Line.perpendicular_of_line c1_c2 p in
  intersect_line c l

let circumscribed p1 p2 p3 =
  let b1 = Line.point_bissection p1 p2
  and b2 = Line.point_bissection p2 p3 in
  let center = Line.intersection b1 b2 in
  let radius = (Point.distance p2 center) in
  make center radius

let incircle p1 p2 p3 =
  let a = Point.distance p1 p2
  and b = Point.distance p2 p3
  and c = Point.distance p3 p1 in
  let center = Point.barycenter [(p1,b);(p2,c);(p3,a)] in
  let radius =
    let s = 0.5 *. (a+.b+.c) in
    (2. *. sqrt (s *. (s-.a) *. (s-.b) *.(s-.c))) /. (a+.b+.c)
  in make center radius

let of_diameter p1 p2 =
  let c = Point.center p1 p2 in
  let radius = (Point.distance p1 c) in
  make c radius

let bounding (pts : Point.t list) : t =
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
  | h::tl -> mindisk pts (make h 0.) [h] tl

let random_point (c:t) : Point.t =
  let theta = Random.float pi *. 2. and r = Random.float (c.radius *. c.radius) |> sqrt  in
  let x = r *. (cos theta) and y = r *. (sin theta) in
  Point.(make (c.center.x+.x) (c.center.y +. y))

let random_point_perimeter (c:t) : Point.t =
  let theta = Random.float pi *. 2. and r = c.radius  in
  let x = r *. (cos theta) and y = r *. (sin theta) in
  Point.(make (c.center.x+.x) (c.center.y +. y))

let print fmt (c:t) =
  Format.fprintf fmt "center:%a, radius=%f" Point.print c.center c.radius
