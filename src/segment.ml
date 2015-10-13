type t = Point.t * Point.t

let make (p1:Point.t) (p2:Point.t) : t = (p1,p2)

let extr1 ((p1,_):t) : Point.t = p1

let extr2 ((_,p2):t) : Point.t = p2
 
let size ((p1,p2):t) = Point.distance p1 p2

let translate ((p1,p2):t) dx dy=
  make (Point.translate p1 dx dy) (Point.translate p2 dx dy)

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
  let (a,b) = proj_x s1 and (c,d) = proj_x s2 in
  if (a<d && b>c) then
    let (a,b) = proj_y s1
    and (c,d) = proj_y s2 in
    (a<d && b>c)
  else false
