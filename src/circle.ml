type t = Point.t * float

let pi = 4.0 *. atan 1.

let make center radius : t= (center,radius)

let center ((c,_):t) = c

let radius ((_,r):t) = r

let translate ((c,r):t) dx dy = 
  make (Point.translate c dx dy) r

let contains ((c,r):t) p = (Point.sq_distance c p) < r

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
  let d = ref (make (Point.make 0. 0.) 0.) in
  let b_md r = match r with
    | [x] -> make x 0.
    | [x;y] -> of_diameter x y
    | [x;y;z] -> circumscribed x y z
    | _ -> failwith "sno"
  in
  let rec b_mindisk p r = 
    match p,r with
    | [],_ |_,[_;_;_] -> b_md r
    | h::tl,_ -> 
      d:=b_mindisk tl r;
      if contains !d h |> not then d:=b_mindisk tl (h::r);
      !d	
  in
  let rec mindisk l : t=
    match l with 
    | [] -> !d
    | h::tl -> 
      d := mindisk tl;
      if contains (!d) h then !d
      else b_mindisk tl [h]
  in 
  match pts with
  | [] -> failwith "can't build a bounding circle with an empty list"
  | [x] -> make x 0.
  | x::tl -> d := (make x 0.); mindisk tl
  



