(**
    The module for the euclidean vector manipulation.
*)
type t = private {
  	dx : float;
  	dy : float;
           }

val make : float -> float -> t

(** null vector *)
val null : t

val x_coord : t -> float

val y_coord : t -> float

(** of_points a b returns the vector begining at a and landing at b *)
val of_points : Point.t -> Point.t -> t

(** returns the norm of a vector *)
val magnitude : t -> float

(** return a vector with same direction but with a norm equal to 1. *)
val normalize : t -> t

val rotation : float -> t -> t

(** dot product of two vectors *)
val dot_product : t -> t -> float

(** scalar product of two vectors*)
val scal_mult : float -> t -> t

val determinant : t -> t -> float

val opposite : t -> t

(** vector addition *)
val add : t -> t -> t

(** vector substraction *)
val substract : t -> t -> t

(** move_to v p returns the translation of p according to the vector v *)
val move_to : t -> Point.t -> Point.t

(** projection v1 v2 returns the projection of v1 on v2*)
val projection : t -> t -> t

(** returns the angle between two vectors*)
val angle : t -> t -> float

(** compute the angle in radians *)
val angle_deg : t -> t -> float
(** compute the angle in degrees *)

(** reflect a b returns the reflected vector of a according to b *)
val reflect : t -> t -> t

val print : Format.formatter -> t -> unit
