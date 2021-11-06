(** The module for the euclidean vector manipulation. *)
type t = private {dx: float; dy: float}

val make : float -> float -> t

val null : t
(** null vector *)

val x_coord : t -> float

val y_coord : t -> float

val of_points : Point.t -> Point.t -> t
(** of_points a b returns the vector begining at a and landing at b *)

val magnitude : t -> float
(** returns the norm of a vector *)

val magnitude_sq : t -> float
(** returns the square of the norm of a vector *)

val normalize : t -> t
(** return a vector with same direction but with a norm equal to 1. *)

val rotation : float -> t -> t

val dot_product : t -> t -> float
(** dot product of two vectors *)

val scal_mult : float -> t -> t
(** scalar product of two vectors*)

val determinant : t -> t -> float

val opposite : t -> t

val add : t -> t -> t
(** vector addition *)

val substract : t -> t -> t
(** vector substraction *)

val move_to : t -> Point.t -> Point.t
(** move_to v p returns the translation of p according to the vector v *)

val projection : t -> t -> t
(** projection v1 v2 returns the projection of v1 on v2*)

val angle : t -> t -> float
(** returns the angle between two vectors*)

(** compute the angle in radians *)
val angle_deg : t -> t -> float
(** compute the angle in degrees *)

val reflect : t -> t -> t
(** reflect a b returns the reflected vector of a according to b *)

val print : Format.formatter -> t -> unit
