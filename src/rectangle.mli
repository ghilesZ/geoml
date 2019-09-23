(** This module provides the basic operations over rectangles *)
module Make: functor
  (A:Arith.T)
  (P:Signatures.Point_Sig   with type arith = A.t)
  (L:Signatures.Line_sig    with type point = P.t and type arith = A.t)
  (S:Signatures.Segment_sig with type point = P.t and type line = L.t) -> sig

  type arith
  type point
  type line
  type segment

  type t = point * arith * arith
  (** rectangle type *)

  val make : point -> arith -> arith -> t
  (** make p w h, returns a rectangle where p is the bottom-left
    corner of the rectangle, w its width and h its height *)

  val of_diagonal : point -> point -> t
  (**  of_diagonal p1 p2, builds the rectangle where p1p2 is its diagonal
     (the bounding rectangle of the two points)*)

  val scale_x : t -> arith -> t
  val scale_y : t -> arith -> t

  (** scale on both axis *)
  val scale : t -> arith -> t

  val bottom_left_corner : t -> point
  val bottom_right_corner : t -> point
  val top_right_corner : t -> point
  val top_left_corner : t -> point

  val translate : arith -> arith -> t -> t
  val point_reflection : point -> t -> t
  val contains : t -> point -> bool
  val area : t -> arith
  val perimeter : t -> arith
  val proj_x : t -> arith * arith
  val proj_y : t -> arith * arith

  val intersects : t -> t -> bool
  (** determines whether or not two rectangles intersect *)

  val intersect_line : t -> line -> point list
  (** returns the intersection points of a rectangle and a line.
    returns [] if they don't intersect.*)

  val segments : t -> segment list
  (** returns a list of length 4 containing the segments of the rectangle*)

  val is_square : t -> bool
  (** tests if the sides of the rectangle have same length *)

  val encompass : t -> point -> t
  (** given a rectangle and point, returns the smallest rectangle that contains the point and the rectangle given as parameters *)

  val bounding : point list -> t
  (** given a list of point, returns the smallest rectangle that contains all the points of the list *)

  val centroid : t -> point
  (** returns the gravity center of a rectangle*)

  val random_point : t -> point
  (** chooses randomly and uniformly a point inside the rectangle *)

  val print : Format.formatter -> t -> unit
  (** printer *)
end
