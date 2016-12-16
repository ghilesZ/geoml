(**
    The module for the euclidean vector manipulation.
*)
type t = private {
  	dx : float;
  	dy : float;
           }

val null : t

val make : float -> float -> t

val x_coord : t -> float

val y_coord : t -> float

(** of_points a b returns the vector begining at a and landing at b *)
val of_points : Point.t -> Point.t -> t

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

val add : t -> t -> t

val substract : t -> t -> t

(** move_to v p returns the translation of p according to the vector v *)
val move_to : t -> Point.t -> Point.t

(** returns the angle between two vectors*)
val angle : t -> t -> float

(** compute the angle in radians *)
val angle_deg : t -> t -> float
  (** compute the angle in degrees *)
