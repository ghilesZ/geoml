(** Triangles manipulation *)

(** the type of triangles *)
type t = private Point.t * Point.t * Point.t

(** tri_map f t applies function f in turn to all the points of t and
    stores the results in a new triangle that is returned. *)
val tri_map : (Point.t -> Point.t) -> t -> t

(** Higher order utilities over the triangles *)
val tri_exists : (Point.t -> bool) -> t -> bool
val tri_find : (Point.t -> bool) -> t -> Point.t
val tri_forall : (Point.t -> bool) -> t -> bool
val tri_iter : (Point.t -> unit) -> t -> unit

(** affine transformation of a triangle *)
val transform : (t -> Affine.t -> t)

(** builds a triangle from three different points.
raises Invalid_arg if at least two points are equal*)
val make : Point.t -> Point.t -> Point.t -> t

(** returns an arbitrary vertice of the triangle.
The vertice returned is different than the one returned by extr2 and extr3.*)
val extr1 : t -> Point.t

(** returns an arbitrary vertice of the triangle.
The vertice returned is different than the one returned by extr1 and extr3.*)
val extr2 : t -> Point.t

(** returns an arbitrary vertice of the triangle.
The vertice returned is different than the one returned by extr1 and extr2.*)
val extr3 : t -> Point.t

val scale_x : t -> float -> t
val scale_y : t -> float -> t

val translate : float -> float -> t -> t
val point_reflection :  Point.t -> t -> t

val contains : t -> Point.t -> bool
(** tests if a point is in a triangle with barycenter method *)

val area : t -> float
val perimeter : t -> float
val proj_x : t -> float * float
val proj_y : t -> float * float
val segments : t -> Segment.t * Segment.t * Segment.t
val intersects : t -> t -> bool
val intersect_line : t -> Line.t -> Point.t list
val is_isoscele : t -> bool
val is_equilateral : t -> bool
val is_right : t -> bool
val points : t -> Point.t * Point.t * Point.t
val as_points : Point.t * Point.t * Point.t -> t
val angles : t -> float * float * float

val centroid : t -> Point.t
(** returns the gravity center of a triangle *)

val random_point : t -> Point.t
(** returns a randomly and uniformly chosen point of the triangle *)

val print : Format.formatter -> t -> unit
(** printer *)
