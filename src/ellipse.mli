type t = private {f1 : Point.t; f2 : Point.t; radius : float
		 ; big_axis : float; small_axis : float}

val make_bifocal : Point.t -> Point.t -> float -> t

val focal1 : t -> Point.t
val focal2 : t -> Point.t
val center : t -> Point.t
val radius : t -> float
val big_axis : t -> float
val small_axis : t -> float

(** radian rotation. rotate e p f returns the rotated ellipse with p
    as the rotation center and f a angle in radian *)
val rotate : t -> Point.t -> float -> t

(** degree rotation. rotate e p f returns the rotated ellipse with p
    as the rotation center and f a angle in degree *)
val rotate_angle : t -> Point.t -> float -> t

val translate : float -> float -> t -> t
val scale_x : t -> float -> t
val scale_y : t -> float -> t
val contains : t -> Point.t -> bool
val area : t -> float
