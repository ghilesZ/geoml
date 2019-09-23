(** Module for 2d points manipulation *)
module Make:
functor (A : Arith.T)
        (Aff:Signatures.Affine_Sig with type arith = A.t)-> sig

  type arith
  type affine

  type t = {
  	  x : arith;
  	  y : arith;
    }

  val make : arith -> arith -> t

  val ( % ) : arith -> arith -> t

  type point = t

  val orig : t

  val center : t -> t -> t

  val determinant : t -> t -> t -> arith

  val iso_barycenter : t list -> t

  val barycenter : (t * arith) list -> t

  val sq_distance : t -> t -> arith

  val distance : t -> t -> arith

  val x_coord : t -> arith

  val y_coord : t -> arith

  val scale_x : t -> arith -> t

  val scale_y : t -> arith -> t

  val translate : arith -> arith -> t -> t

  val transform : affine -> t -> t

  (** point reflection. reflection p1 p2 returns the symerical point of p2 with respect to p1 *)
  val point_reflection : t -> t -> t

  (** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1 as center and f a angle in radian *)
  val rotate : t -> t -> arith -> t

  (** point rotation. rotate p1 p2 f returns the rotation point of p2 with p1 as center and f a angle in degree *)
  val rotate_angle : t -> t -> arith -> t

  val print : Format.formatter -> t -> unit
end
