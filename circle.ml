type t = Point.t * float

let pi = 4.0 *. atan 1.

let make center radius = (center,radius)

let contains (c,r) p = (Point.sq_distance c p) < r

let area (_,r) = r *. r *. pi
