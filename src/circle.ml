type t = Point.t * float

let pi = 4.0 *. atan 1.

let make center radius : t= (center,radius)

let center ((c,_):t) = c

let radius ((_,r):t) = r

let translate ((c,r):t) dx dy = 
  make (Point.translate c dx dy) r

let contains ((c,r):t) p = (Point.sq_distance c p) < r *. r

let area ((_,r):t) = pi *. r *. r

let perimeter ((_,r):t) = 2. *. pi *. r

let proj_x ((c,r):t) = let open Point in (c.x-.r,c.x+.r)

let proj_y ((c,r):t) = let open Point in (c.y-.r,c.y+.r)

let intersects ((c1,r1):t) ((c2,r2):t) = 
  (Point.sq_distance c1 c2) < (r1 +. r2) ** 2.

let circumscribed p1 p2 p3 =
  let b1 = Line.bissection p1 p2
  and b2 = Line.bissection p2 p3 in
  let angle = Vector.angle (Vector.of_points p1 p2) (Vector.of_points p1 p3) in
  let center = Line.intersection b1 b2 
  and radius = (Point.distance p2 p3) /. 
    (2. *. (sin angle))
  in make center radius

let of_diameter p1 p2 = make (Point.center p1 p2) (0.5 *. Point.distance p1 p2)

     
(** given a list of point, returns the smallest circle that
   contains all the points of the list *)
let bounding (pts : Point.t list) : t =	
  let update set pt =
    match set with
    | [x] -> [x;pt],(of_diameter x pt)
    | [x;y] ->
       let c = of_diameter pt x in
       if contains c y then ([x;pt],c) else
	 let c = of_diameter pt y in
	 if contains c x then ([y;pt],c)
	 else ([x;y;pt],circumscribed pt x y)
    | [x;y;z] ->
       let contains2 c a b = contains c a && contains c b in
       let c = (of_diameter pt x) in 
       if contains2 c y z then ([x;pt],c) else
	 let c = (of_diameter pt y) in
	 if contains2 c x z then ([y;pt],c) else
	   let c = (of_diameter pt z) in
           if contains2 c x y then ([z;pt],c) else
	     let c1 = circumscribed pt x y
	     and c2 = circumscribed pt x z
	     and c3 = circumscribed pt y z in
	     let r1 = radius c1 and r2 = radius c2 and r3 = radius c3 in
	     match ((contains c1 z),(contains c2 y),(contains c3 x)) with
	     |(true,true,true) when r1 < r2 && r1 < r3 -> ([pt;x;y],c1)
	     |(true,true,true) when r2 < r1 && r2 < r3 -> ([pt;x;z],c2)
	     |(true,true,true) when r1 < r2 && r1 < r3 -> ([pt;z;y],c3)
	     |(_,true,true) when r2 < r3 -> ([pt;x;z],c2)
	     |(_,true,true) -> ([pt;z;y],c3)
	     |(true,_,true) when r1 < r3 -> ([pt;x;y],c1)
	     |(true,_,true) -> ([pt;z;y],c3)
	     |(true,true,_) when r1 < r2 -> ([pt;x;y],c1)
	     |(true,true,_) -> ([pt;x;z],c2)
	     |(true,_,_) -> ([pt;x;y],c1)
	     |(_,true,_) -> ([pt;x;z],c2)
	     |(_,_,true) -> ([pt;z;y],c3)
  in
  let rec mindisk circle set pts =
    match pts with
    | [] -> circle
    | h::tl when (contains circle h |> not) && (List.mem h set |> not) ->
       let (new_set,new_circle) = update set h in
       mindisk new_circle new_set tl
    | h::tl -> mindisk circle set tl
  in
  match pts with
  | [] -> failwith "can't build a bounding circle with an empty list"
  | h::tl -> mindisk (make h 0.) [h] tl
