(** Ellipses manipulation *)
module Make:
functor (A : Arith.T)
          (P:Signatures.Point_Sig with type arith = A.t)-> sig

type arith
type point


type t = {f1 : point; f2 : point; radius : arith
		 ; big_axis : arith; small_axis : arith}

val make_bifocal : point -> point -> arith -> t

val focal1 : t -> point
val focal2 : t -> point
val center : t -> point
val radius : t -> arith
val big_axis : t -> arith
val small_axis : t -> arith

(** radian rotation. rotate e p f returns the rotated ellipse with p
    as the rotation center and f a angle in radian *)
val rotate : t -> point -> arith -> t

(** degree rotation. rotate e p f returns the rotated ellipse with p
    as the rotation center and f a angle in degree *)
val rotate_angle : t -> point -> arith -> t

val translate : arith -> arith -> t -> t
val scale_x : t -> arith -> t
val scale_y : t -> arith -> t
val contains : t -> point -> bool
val area : t -> arith

end
