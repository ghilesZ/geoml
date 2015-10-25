type t = Point.t * float

let pi = 4.0 *. atan 1.

let make center radius : t= (center,radius)

let center ((c,_):t) = c

let radius ((_,r):t) = r

let translate ((c,r):t) dx dy = 
  make (Point.translate c dx dy) r

let contains ((c,r):t) p =
  let d1 = Point.sq_distance c p
  and d2 = (r*.r) in
  d1 < d2

let area ((_,r):t) = pi *. r *. r

let perimeter ((_,r):t) = 2. *. pi *. r

let proj_x ((c,r):t) = let open Point in (c.x-.r,c.x+.r)

let proj_y ((c,r):t) = let open Point in (c.y-.r,c.y+.r)

let intersects ((c1,r1):t) ((c2,r2):t) = 
  (Point.sq_distance c1 c2) < (r1 +. r2) ** 2.

let line_intersection ((c,r):t) (l:Line.t) =
  let cx = Point.x_coord c and cy = Point.y_coord c in
  match l with
  | Line.Y(a,b) ->
    (* we go to origin *)
    let l_2 = Line.translate l (-.cx) (-.cy) in
    let a = Line.get_coeff l_2 and b = Line.get_ord l_2 in
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
  | Line.X(x) ->
    let a = x-.cx in
    Math.solve 1. 0. (a*.a -. r*.r)
  |> List.map (fun y -> Point.make x y)
  |> List.map (fun p -> Point.translate p 0. cy)

let intersection (((c1,_) as c):t) (((c2,_)as c'):t) = 
  let c1_c2 = Line.of_points c1 c2 in
  let p = Point.barycenter [c;c'] in
  let l = Line.perpendicular_of_line c1_c2 p in
  line_intersection c l

let circumscribed p1 p2 p3 =
  let b1 = Line.point_bissection p1 p2
  and b2 = Line.point_bissection p2 p3 in
  let angle = Vector.angle (Vector.of_points p1 p2) (Vector.of_points p1 p3) in
  let center = Line.intersection b1 b2 
  and radius = (Point.distance p2 p3) /. 
    (2. *. (sin angle))
  in make center radius

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
     
(** given a list of point, returns the smallest circle that
   contains all the points of the list, using emo welzl's algorithm.
    complexity in expected linear time*)
let bounding (pts : Point.t list) : t =
  let of_two x y pt = 
    let cx = of_diameter pt x in
    if contains cx y then ([x;pt],cx) else
      let cy = of_diameter pt y in
      if contains cy x then ([y;pt],cy)
      else raise Not_found
  in
  let of_three x y z pt =
    let try_2 a b contained = 
      let c = (of_two a b pt) in
      if contains (snd c) contained then c
      else raise Not_found
    in
    try 
      try (try_2 x y z) 
      with Not_found -> 
	try (try_2 x z y) 
	with Not_found -> (try_2 y z x)
    with
    (* the solution goes through three points *)
    | Not_found -> begin
      (* returns the indice of the smallest value *)
      let smallest a b c = 
	let res = ref (a,0) in
	if b < a then res := (b,1);
	if c < fst (!res) then 2
	else snd (!res)
      in
      let ((_,r1) as c1) = circumscribed pt x y
      and ((_,r2) as c2) = circumscribed pt x z
      and ((_,r3) as c3) = circumscribed pt y z
      and s1 = [pt;x;y] and s2 = [pt;x;z] and s3 = [pt;y;z] in
      let res_array = [|(s1,c1);(s2,c2);(s3,c3)|] in
      (match ((contains c1 z),(contains c2 y),(contains c3 x)) with
      |(true,false,false) -> res_array.(0)
      |(false,true,false) -> res_array.(1)
      |(false,false,true) -> res_array.(2)
      |(false,true,true)  -> res_array.(smallest (r3+.1.) r2 r3)
      |(true,false,true)  -> res_array.(smallest r1 (r1+.1.) r3)
      |(true,true,false)  -> res_array.(smallest r1 r2 (r2+.1.))
      |(true,true,true)   -> res_array.(smallest r1 r2 r3)
      | _ -> assert false)
    end
  in 
  let update set pt =
    match set with
    | [x] -> [x;pt],(of_diameter x pt)
    | [x;y] -> 
      (try of_two x y pt
      with Not_found -> ([x;y;pt],circumscribed pt x y))
    | [x;y;z] -> of_three x y z pt
    | _ -> assert false  
  in
  let rec mindisk circle set pts =
    match pts with
    | [] -> circle
    | h::tl when contains circle h ->  mindisk circle set tl
    | h::tl -> 
      let (new_set,new_circle) = update set h in
      mindisk new_circle new_set tl
  in
  match pts with
  | [] -> failwith "can't build a bounding circle with an empty list"
  | h::tl -> mindisk (make h 0.) [h] tl
