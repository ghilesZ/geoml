
type t = private Point.t * float

val make : Point.t -> float -> t
val center : t -> Point.t
val radius : t -> float
val translate : t -> float -> float -> t
val point_reflection : Point.t -> t -> t
val contains : t -> Point.t -> bool
val area : t -> float
val perimeter : t -> float
val proj_x : t -> float * float
val proj_y : t -> float * float
val intersects : t -> t -> bool
val line_intersection : t -> Line.t -> Point.t list
val intersection : t -> t -> Point.t list
val circumscribed : Point.t -> Point.t -> Point.t -> t
val incircle : Point.t -> Point.t -> Point.t -> t
val of_diameter : Point.t -> Point.t -> t

val bounding : Point.t list -> t
(** given a list of point, returns the smallest circle that
   contains all the points of the list, using emo welzl's algorithm.
    complexity in expected linear time *)
