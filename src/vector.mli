(** The module for the euclidean vector manipulation. *)
module Make:
functor (A : Arith.T)
          (P:Signatures.Point_Sig with type arith = A.t)-> sig

  type arith
  type point

  type t = {
  	  dx : arith;
  	  dy : arith;
    }

  val make : arith -> arith -> t

  (** null vector *)
  val null : t

  val x_coord : t -> arith

  val y_coord : t -> arith

  (** of_points a b returns the vector begining at a and landing at b *)
  val of_points : point -> point -> t

  (** returns the norm of a vector *)
  val magnitude : t -> arith

  (** return a vector with same direction but with a norm equal to 1. *)
  val normalize : t -> t

  val rotation : arith -> t -> t

  (** dot product of two vectors *)
  val dot_product : t -> t -> arith

  (** scalar product of two vectors*)
  val scal_mult : arith -> t -> t

  val determinant : t -> t -> arith

  val opposite : t -> t

  (** vector addition *)
  val add : t -> t -> t

  (** vector substraction *)
  val substract : t -> t -> t

  (** move_to v p returns the translation of p according to the vector v *)
  val move_to : t -> point -> point

  (** projection v1 v2 returns the projection of v1 on v2*)
  val projection : t -> t -> t

  (** returns the angle between two vectors*)
  val angle : t -> t -> arith

  (** compute the angle in radians *)
  val angle_deg : t -> t -> arith
  (** compute the angle in degrees *)

  (** reflect a b returns the reflected vector of a according to b *)
  val reflect : t -> t -> t

  val print : Format.formatter -> t -> unit
end
