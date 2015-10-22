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

let intersection ((c,r):t) (l:Line.t) =
  let a',b' = c.Point.x,c.Point.y in
  match l with
  | Line.Y(a,b) ->
     let grand_b = b -. b' in
     let delta = r*.r *. (1. +. a*.a) -. (a *. a' +. grand_b *. grand_b) in
     print_string "a' ";print_float a';print_newline();
     print_string "b' "; print_float b';print_newline(); 
     print_string "a "; print_float a;print_newline();
     print_string "b "; print_float b;print_newline();	  
     print_string "delta "; print_float delta; print_newline();
     if delta < 0. then [] else 
	 let sol1 = ((-.a' +. a *. grand_b) +. (sqrt delta)) /. (1. +. a*.a) in
	 if delta = 0. then [Point.make sol1 (Line.y_from_x l sol1)]
	 else let sol2 = ((-.a' +. a*.grand_b) -. (sqrt delta)) /. (1.+.a*.a) in
	      [Point.make sol1 (Line.y_from_x l sol1);
	       Point.make sol2 (Line.y_from_x l sol2)]
  | Line.X(x) ->
     let a,b = c.Point.x,c.Point.y in
     let c = r*.r -. b*.b -. x*.x -. 2.*.a*.x +. a*.a
     in let delta = 4. *. (b*.b -. c) in
	if delta < 0. then [] else 
	  let sol1 = (2. *. b -. (sqrt delta)) /. 2. in
	  if delta = 0. then [Point.make x sol1]
	  else let sol2 = (2. *. b +. (sqrt delta)) /. 2. in
	       [Point.make x sol1;
		Point.make x sol2]

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
	     let smallest a b c = a<b && a<c in
	     let ((_,r1) as c1) = circumscribed pt x y
	     and ((_,r2) as c2) = circumscribed pt x z
	     and ((_,r3) as c3) = circumscribed pt y z 
	     and s1 = [pt;x;y] and s2 = [pt;x;z] and s3 = [pt;y;z] in
	     (match ((contains c1 z),(contains c2 y),(contains c3 x)) with
	     |(true,true,true) when smallest r1 r2 r3 -> (s1,c1)
	     |(true,true,true) when smallest r2 r1 r3 -> (s2,c2)
	     |(true,true,true) -> (s3,c3)
	     |(_,true,true) when r2 < r3 -> (s2,c2)
	     |(_,true,true) -> (s3,c3)
	     |(true,_,true) when r1 < r3 -> (s1,c1)
	     |(true,_,true) -> (s3,c3)
	     |(true,true,_) when r1 < r2 -> (s1,c1)
	     |(true,true,_) -> (s2,c2)
	     |(true,_,_) -> (s1,c1)
	     |(_,true,_) -> (s2,c2)
	     |(_,_,true) -> (s3,c3)
	     | _ -> assert false)
    | _ -> assert false
	    
  in
  let rec mindisk circle set pts =
    match pts with
    | [] -> circle
    | h::tl when (contains circle h |> not) ->
       let (new_set,new_circle) = update set h in
       mindisk new_circle new_set tl
    | h::tl -> mindisk circle set tl
  in
  match pts with
  | [] -> failwith "can't build a bounding circle with an empty list"
  | h::tl -> mindisk (make h 0.) [h] tl
