type t = Point.t * float

let pi = 4.0 *. atan 1.

let make center radius = (center,radius)

let contains (c,r) p = (Point.sq_distance c p) < r

let area (_,r) = pi *. r *. r

let perimeter (_,r) = 2. *. pi *. r

let intersects (c1,r1) (c2,r2) = 
  (Point.sq_distance c1 c2) < (r1 +. r2) ** 2. 
