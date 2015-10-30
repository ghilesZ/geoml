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
val translate : t -> float -> float -> t
val deg_to_rad : float
val rotate : t -> t -> float -> t
val rotate_angle : t -> t -> float -> t
val print : Format.formatter -> t -> unit
