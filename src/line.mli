type t = private 
|	X of float
|	Y of float * float	(* linear equation type *)

type error =
|	Vertical of float
|	Parallel
|	Same_coordinates of Point.t
exception Error of error
val print_error : Format.formatter -> error -> unit

val make_x : float -> t
val make_y : float -> float -> t
val x_axis : t
val y_axis : t
val is_vertical : t -> bool
val get_coeff : t -> float
val get_ord : t -> float
val to_string : t -> string
val of_points : Point.t -> Point.t -> t
val x_from_y : t -> float -> float
val y_from_x : t -> float -> float
val contains : t -> Point.t -> bool
val translate : t -> float -> float -> t
val parallel : t -> t -> bool
val intersects : t -> t -> bool
val intersection : t -> t -> Point.t
val perpendicular : t -> t -> bool
val perpendicular_of_line : t -> Point.t -> t
val parallel_of_line : t -> Point.t -> t
val orth_proj : t -> Point.t -> Point.t
val point_bissection : Point.t -> Point.t -> t
