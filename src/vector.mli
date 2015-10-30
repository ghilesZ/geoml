
type t = private {
  	dx : float;
  	dy : float;
}

val make : float -> float -> t
val x_coord : t -> float
val y_coord : t -> float
val of_points : Point.t -> Point.t -> t
val magnitude : t -> float
val normalize : t -> t
val dot_product : t -> t -> float
val scal_mult : t -> float -> t
val determinant : t -> t -> float
val opposite : t -> t
val add : t -> t -> t
val substract : t -> t -> t
val move_to : t -> Point.t -> Point.t
val angle : t -> t -> float
val angle_deg : t -> t -> float
