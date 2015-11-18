open Math

type t = Point.t * float

let make center radius : t= (center,radius)

let center ((c,_):t) = c

let radius ((_,r):t) = r

let translate ((c,r):t) dx dy = 
  make (Point.translate c dx dy) r

let point_reflection p ((c,r):t) =
  let p' = Point.point_reflection p c in
  make p' r

let contains ((c,r):t) p =
  Point.sq_distance c p <= (r*.r)

let area ((_,r):t) = pi *. r *. r

let perimeter ((_,r):t) = 2. *. pi *. r

let proj_x ((c,r):t) = let open Point in (c.x-.r,c.x+.r)

let proj_y ((c,r):t) = let open Point in (c.y-.r,c.y+.r)

let intersects ((c1,r1):t) ((c2,r2):t) = 
  (Point.sq_distance c1 c2) < (r1 +. r2) ** 2.
    
(** line_intersection takes a circle and line and returns the list of the 
    intersection points. (can be [], [a] or [a,b]
 *)
let line_intersection ((c,r):t) (l:Line.t) =
  let cx = Point.x_coord c and cy = Point.y_coord c in
  let open Line in
  match l with
  | Y(a,b) ->
     (* we go to origin *)
     let l_2 = translate l (-.cx) (-.cy) in
     let a,b = (match l_2 with Y(a,b) -> a,b | _ -> assert false) in
    (* we solve the equation at the origin for x*)
    (* x² + y² = r²   and   y = ax+b  
       => x² + (ax + b)² = r²
       => x² + a²x² + 2abx + b² = r²
       => (a²+1)x² + 2abx + b²-r² = 0 *)
    Math.solve (a*.a+.1.) (2.*.a*.b) (b*.b -. r*.r)
     (* we calculate the associated y*)
  |> List.map (fun x -> Point.make x (a*.x+.b))
    (* we translate the result to the first coordinates*)
  |> List.map (fun x -> Point.translate x cx cy)
  | X(x) ->
    let a = x-.cx in
    Math.solve 1. 0. (a*.a -. r*.r)
  |> List.map (fun y -> Point.make x y)
  |> List.map (fun p -> Point.translate p 0. cy)


(** tangent c p returns the tangent of circle c going through point p. 
    p must lie on c's boundary
 *)
let tangent ((c,r):t) p =
  Line.perpendicular_of_line (Line.of_points c p) p

let intersection (((c1,_) as c):t) (((c2,_)as c'):t) = 
  let c1_c2 = Line.of_points c1 c2 in
  let p = Point.barycenter [c;c'] in
  let l = Line.perpendicular_of_line c1_c2 p in
  line_intersection c l

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
      |> List.find (fun ((env,circle),inner) -> contains circle inner)
      |> fst
    with Not_found -> ([x;y;pt],(circumscribed x y pt))
  in
  let of_three x y z pt =
    let found =
      [((x,y),z); ((x,z),y); ((y,z),x)]
      |> List.map (fun ((a,b),c) -> ((of_two a b pt),c))
      |> List.filter (fun ((e,c),inner) -> contains c inner)
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
    | h::tl -> 
       let (new_set,new_circle) = update set h in
       List.filter (fun e -> List.mem e new_set |> not) l |>
       mindisk l new_circle new_set
  in
  match pts with
  | [] -> failwith "can't build a bounding circle with an empty list"
  | h::tl -> mindisk pts (make h 0.) [h] tl

let print fmt ((c,r):t) = Format.fprintf fmt "center:%a, radius=%f" Point.print c r

