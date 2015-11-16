type t = Point.t * Point.t 
val make : Point.t -> Point.t -> t

module Tbl: Hashtbl.S with type key = t

val extr1 : t -> Point.t
val extr2 : t -> Point.t
val center : t -> Point.t
val size : t -> float
val translate : t -> float -> float -> t
val to_line : t -> Line.t
val contains : t -> Point.t -> bool
val proj_x : t -> float * float
val proj_y : t -> float * float
val intersects : t -> t -> bool
val intersection : t -> t -> Point.t option
val intersect_line : t -> Line.t -> Point.t option
