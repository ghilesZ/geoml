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
(** intersects a b, returns true if a and b intersect. false otherwise*)
  
val intersection : t -> t -> Point.t option
(** returns the intersection point of two segments.
    returns None if they don't intersect*)
  
val intersect_line : t -> Line.t -> Point.t option
(** returns the intersection point of a segment and a line.
    returns None if they don't intersect*)
