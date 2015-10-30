(** This module provides the basic operations over rectangles *)

type t = private Point.t * float * float
(** rectangle type *)

val make : Point.t -> float -> float -> t
(** where p is the bottom-left corner of the rectangle *)

val bottom_left_corner : t -> Point.t
val bottom_right_corner : t -> Point.t
val top_right_corner : t -> Point.t
val top_left_corner : t -> Point.t
val translate : t -> float -> float -> t
val contains : t -> Point.t -> bool
val area : t -> float
val perimeter : t -> float
val proj_x : t -> float * float
val proj_y : t -> float * float

val intersects : t -> t -> bool
(** determines whether or not two rectangles intersect *)

val segments : t -> Segment.t list
val is_square : t -> bool

val encompass : Point.t * float * float -> Point.t -> Point.t * float * float
(** given a rectangle and point, returns the smallest rectangle that contains the point and the rectangle given as parameters *)

val bounding : Point.t list -> t
(** given a list of point, returns the smallest rectangle that contains all the points of the list *)
