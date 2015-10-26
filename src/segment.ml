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

let intersection (s1:t) (s2:t) =
  try
    let p = Line.intersection (to_line s1) (to_line s2) in
    if contains s1 p && contains s2 p then Some p
    else None
  with
  | Line.Error Line.Parallel -> None
