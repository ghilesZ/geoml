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
