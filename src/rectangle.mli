(** This module provides the basic operations over rectangles *)

(** rectangle type *)
type t = private Point.t * float * float

val make : Point.t -> float -> float -> t
(** make p w h, returns a rectangle where p is the bottom-left corner of the
    rectangle, w its width and h its height *)

val of_diagonal : Point.t -> Point.t -> t
(** of_diagonal p1 p2, builds the rectangle where p1p2 is its diagonal (the
    bounding rectangle of the two points)*)

val scale_x : t -> float -> t

val scale_y : t -> float -> t

val scale : t -> float -> t
(** scale on both axis *)

val bottom_left_corner : t -> Point.t

val bottom_right_corner : t -> Point.t

val top_right_corner : t -> Point.t

val top_left_corner : t -> Point.t

val translate : float -> float -> t -> t

val point_reflection : Point.t -> t -> t

val contains : t -> Point.t -> bool

val area : t -> float

val perimeter : t -> float

val proj_x : t -> float * float

val proj_y : t -> float * float

val intersects : t -> t -> bool
(** determines whether or not two rectangles intersect *)

val intersect_line : t -> Line.t -> Point.t list
(** returns the intersection points of a rectangle and a line. returns [] if
    they don't intersect.*)

val segments : t -> Segment.t list
(** returns a list of length 4 containing the segments of the rectangle*)

val is_square : t -> bool
(** tests if the sides of the rectangle have same length *)

val encompass : t -> Point.t -> t
(** given a rectangle and point, returns the smallest rectangle that contains
    the point and the rectangle given as parameters *)

val bounding : Point.t list -> t
(** given a list of point, returns the smallest rectangle that contains all
    the points of the list *)

val centroid : t -> Point.t
(** returns the gravity center of a rectangle*)

val random_point : Random.State.t -> t -> Point.t
(** returns a randomly and uniformly chosen point of the rectangle *)

val print : Format.formatter -> t -> unit
(** printer *)
