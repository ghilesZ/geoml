(** This module provides basic functions for any concave or convexe polygon (Does not handle complex
    polygon and polygon with holes) *)

type t = private Point.t list 
type polygon = t

val make : Point.t list -> t
val first_point : t -> Point.t
val to_list : t -> Point.t list

val fold : ('a -> Point.t -> Point.t -> 'a) -> 'a -> t -> 'a
(** Same function as [List.fold_left] but on segments of a given polygon *)

val fold_filter : (Point.t -> Point.t -> bool) -> ('b -> Point.t -> Point.t -> 'b) -> 'b -> t -> 'b
(** Same function as [fold] but filters segments with the first argument *)

val map : (Point.t -> Point.t) -> t -> t

val perimeter : t -> float
val area : t -> float
val proj_x : t -> float * float
val proj_y : t -> float * float
val translate : float -> float -> t -> t
val transform : Affine.t -> t -> t
val bounding : Point.t list -> t
val minmax_xy : t -> float * float * float * float

val contains : t -> Point.t -> bool
  (** Returns true if a polygon contains a point.
      Randolph Franklin code for raycasting *)

val segments_intersection_points : Point.t list Segment.Tbl.t -> t -> t -> Point.t list
  (** Get a list of the intersections
    points of the edges of two polygons *)

val intersection_polygons : t -> t -> t list
  (** Implementation of Weiler Atherton Algorithm for
    concave/convexe polygons clipping. Complexity is O(m*n). *)

val triangulation : t -> (Point.t * Point.t * Point.t) list

module Regular: sig
  (** Module for Regular polygons *)

  type t = private {
    center : Point.t;
    fst : Point.t;
    snd : Point.t;
    len : float;
    apothem : float; (** Distance between the center and the middle of one of the edges *)
    edges : int;
  }

  val make : Point.t -> Point.t -> int -> t
  (** Create a regular polygon from the center, an arbitrary point and the
      number of edges *)

  val next_point : ?nth:int -> t -> Point.t
  val fold_stop : (Point.t -> Point.t -> bool) ->
    (int -> 'a -> Point.t -> Point.t -> 'a) -> 'a -> t -> 'a
  val perimeter : t -> float
  val area : t -> float
  val to_polygon : t -> polygon
  val to_randomized_polygon : ?minp:int -> ?prob:float -> t -> polygon
  val translate : float -> float -> t -> t
  val transform : Affine.t -> t -> t
  val is_square : t -> bool
  val contains : t -> Point.t -> bool
  val map : (Point.t -> Point.t) -> t -> t

end
