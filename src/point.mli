type t = private {
  	x : float;
  	y : float;
}

val make : float -> float -> t
type point = t 

module Tbl: Hashtbl.S with type key = t

val orig : t

val center : t -> t -> t

val determinant : t -> t -> t -> float

val iso_barycenter : t list -> t

val barycenter : (t * float) list -> t

val sq_distance : t -> t -> float

val distance : t -> t -> float

val x_coord : t -> float

val y_coord : t -> float

val scale_x : t -> float -> t

val scale_y : t -> float -> t

val translate : float -> float -> t -> t

val transform : Affine.t -> t -> t

(** point reflection. reflection p1 p2 returns the symerical point of p2 with respect to p1 *)
val point_reflection : t -> t -> t

(** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1 as center and f a angle in radian *)
val rotate : t -> t -> float -> t

(** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1 as center and f a angle in degree *)
val rotate_angle : t -> t -> float -> t
val print : Format.formatter -> t -> unit
