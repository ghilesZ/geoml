type t = private Point.t * Point.t * Point.t 

val print : Format.formatter -> t -> unit

val tri_map : (Point.t -> Point.t) -> t -> t
val tri_exists : (Point.t -> bool) -> t -> bool
val tri_find : ('a -> bool) -> 'a * 'a * 'a -> 'a
val tri_forall : ('a -> bool) -> 'a * 'a * 'a -> bool
val tri_iter : ('a -> 'b) -> 'a * 'a * 'a -> 'b
val make : Point.t -> Point.t -> Point.t -> t
val extr1 : t -> Point.t
val extr2 : t -> Point.t
val extr3 : t -> Point.t
val translate : t -> float -> float -> t

val contains : t -> Point.t -> bool
(** tests if a point is in a triangle with barycenter method *)

val area : t -> float
val perimeter : t -> float
val proj_x : t -> float * float
val proj_y : t -> float * float
val segments : t -> Segment.t * Segment.t * Segment.t
val intersects : t -> t -> bool
val is_isoscele : t -> bool
val is_equilateral : t -> bool
val is_right : t -> bool
val points : t -> Point.t * Point.t * Point.t
val as_points : Point.t * Point.t * Point.t -> t
val angles : t -> float * float * float
