type t = Point.t * float

let pi = 4.0 *. atan 1.

let make center radius : t= (center,radius)

let contains ((c,r):t) p = (Point.sq_distance c p) < r

let area ((_,r):t) = pi *. r *. r

let perimeter ((_,r):t) = 2. *. pi *. r

let intersects ((c1,r1):t) ((c2,r2):t) = 
  (Point.sq_distance c1 c2) < (r1 +. r2) ** 2.
