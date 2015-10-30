
type t = private Point.t list 
type polygon = t
val make : Point.t list -> t
val first_point : t -> Point.t
val to_list : t -> Point.t list
val fold : ('a -> Point.t -> Point.t -> 'a) -> 'a -> t -> 'a
val fold_filter : (Point.t -> Point.t -> bool) -> ('b -> Point.t -> Point.t -> 'b) -> 'b -> t -> 'b
val perimeter : t -> float
val area : t -> float
val proj_x : t -> float * float
val proj_y : t -> float * float
val bounding : Point.t list -> t
val minmax_xy : t -> float * float * float * float
val contains : t -> Point.t -> bool
val segments_intersection_points : Point.t list Segment.Tbl.t -> t -> t -> Point.t list
val intersection_polygons : t -> t -> bool * Point.t list * t list

module Regular: sig
  type t = {
    center : Point.t;
    fst : Point.t;
    snd : Point.t;
    len : float;
    apothem : float;
    edges : int;
  }
  val make : Point.t -> Point.t -> int -> t
  val next_point : ?nth:int -> t -> Point.t
  val fold_stop : (Point.t -> Point.t -> bool) ->
    (int -> 'a -> Point.t -> Point.t -> 'a) -> 'a -> t -> 'a
  val perimeter : t -> float
  val area : t -> float
  val to_polygon : t -> polygon
  val to_randomized_polygon : ?minp:int -> ?prob:float -> t -> polygon
  val translate : t -> float -> float -> t
  val is_square : t -> bool
  val contains : t -> Point.t -> bool
end
