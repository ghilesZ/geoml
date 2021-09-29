(** Module for 2d points manipulation *)

type t = private {x: float; y: float}

val make : float -> float -> t

val ( % ) : float -> float -> t

module Tbl : Hashtbl.S with type key = t

val orig : t
(** the point [{x=0.; y=0.}]*)

val center : t -> t -> t

val determinant : t -> t -> t -> float

val iso_barycenter : t list -> t

val barycenter : (t * float) list -> t

val sq_distance : t -> t -> float
(** square distance between two points *)

val distance : t -> t -> float
(** distance *)

val x_coord : t -> float

val y_coord : t -> float

val scale_x : t -> float -> t

val scale_y : t -> float -> t

val translate : float -> float -> t -> t

val transform : Affine.t -> t -> t

val reflection : t -> t -> t
(** point reflection. reflection p1 p2 returns the symerical point of p2 with
    respect to p1 *)

val rotate : t -> t -> float -> t
(** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1
    as center and f a angle in radian *)

val rotate_angle : t -> t -> float -> t
(** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1
    as center and f a angle in degree *)

val print : Format.formatter -> t -> unit
(** printer *)
