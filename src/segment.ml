type t = Point.t * Point.t

let make (p1:Point.t) (p2:Point.t) : t = (p1,p2)

let extr1 ((p1,_):t) : Point.t = p1

let extr2 ((_,p2):t) : Point.t = p2
 
let size ((p1,p2):t) = Point.distance p1 p2

let translate ((p1,p2):t) dx dy=
  make (Point.translate p1 dx dy) (Point.translate p2 dx dy)

let to_line ((p1,p2):t) = Line.of_points p1 p2

let contains ((a,b):t) p = 
  Point.sq_distance a p +. Point.sq_distance p b =  Point.sq_distance a b

let proj_x ((a,b):t) = 
  let open Point in
  if a.x > b.x then b.x,a.x
  else a.x,b.x

let proj_y ((a,b):t) = 
  let open Point in
  if a.y > b.y then b.y,a.y
  else a.y,b.y



let intersects (s1:t) (s2:t) = 
  try 
    let p = Line.intersection (to_line s1) (to_line s2) in
    contains s1 p && contains s2 p
  with 
  | Line.Error Line.Parallel -> false

let intersection (a1, b1 as s1) (a2, b2 as s2) =
  let open Point in
  try
    let p = Line.intersection (to_line s1) (to_line s2) in
    if
      max (min a1.x b1.x) (min a2.x b2.x) < p.x
      && min (max a1.x b1.x) (max a2.x b2.x) > p.x
      && max (min a1.y b1.y) (min a2.y b2.y) < p.y
      && min (max a1.y b1.y) (max a2.y b2.y) > p.y
    then Some p else None
  with
  | Line.Error Line.Parallel -> None
