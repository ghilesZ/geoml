type t = Point.t * Point.t

let make (p1:Point.t) (p2:Point.t) : t = (p1,p2)

module Tbl = Hashtbl.Make (struct
    type t = Point.t * Point.t
    let equal (a1,b1) (a2, b2) = a1 == a2 && b1 == b2
    let hash = Hashtbl.hash
  end)

let extr1 ((p1,_):t) : Point.t = p1

let extr2 ((_,p2):t) : Point.t = p2

let sq_size ((p1,p2):t) = Point.sq_distance p1 p2

let size ((p1,p2):t) = Point.distance p1 p2

let map f (p1, p2) = (f p1, f p2)

let center (p1,p2) = Point.center p1 p2

let equation ((p1,p2):t) t =
  if t < 0. || t > 1. then
    invalid_arg "Segment.equation: parameter must be in [0. ; 1.]"
  else
    let open Point in
    let (dx,dy) = (p2.x -. p1.x),(p2.y -. p1.y) in
    Point.translate (t*.dx) (t*.dy) p1

let scale_y (p1,p2) f = make (Point.scale_y p1 f) (Point.scale_y p2 f)

let scale_x (p1, p2) f = make (Point.scale_x p1 f) (Point.scale_x p2 f)

let translate dx dy = map (Point.translate dx dy)

let transform m = map Point.(transform m)

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

let intersects ((a1, b1 as s1):t) ((a2,b2 as s2):t) =
  let open Point in
  try
    let p = Line.intersection (to_line s1) (to_line s2)
    and sqd = sq_distance a1 b1 
    and sqd2 = sq_distance a2 b2 in
    sq_distance a1 p <= sqd && sq_distance b1 p <= sqd &&
    sq_distance a2 p <= sqd2 && sq_distance b2 p <= sqd2
  with
  | Line.Error Line.Parallel(_) -> false

let intersection ((a1, b1 as s1):t) ((a2,b2 as s2):t) =
  let open Point in
  try
    let p = Line.intersection (to_line s1) (to_line s2)
    and sqd = sq_distance a1 b1 
    and sqd2 = sq_distance a2 b2 in
    if sq_distance a1 p < sqd && sq_distance b1 p < sqd &&
       sq_distance a2 p < sqd2 && sq_distance b2 p < sqd2
    then Some p else None
  with
  | Line.Error Line.Parallel(_) -> None

let intersect_line (((p1,p2) as s):t) l =
  let open Point in
  try
    let p = Line.intersection l (to_line s)
    and sqd = sq_distance p1 p2 in
    if sq_distance p1 p <= sqd && sq_distance p2 p <= sqd 
    then Some p else None
  with
  | Line.Error Line.Parallel(_) -> None
