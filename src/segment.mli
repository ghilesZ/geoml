(** Segment manipulation *)
module Make:
functor (A : Arith.T)
          (P:Signatures.Point_Sig with type arith = A.t)
          (L:Signatures.Line_sig with type point = P.t) -> sig

  type arith
  type affine
  type line
  type point

  type t = point * point

  val make : point -> point -> t

  val extr1 : t -> point

  val extr2 : t -> point

  val center : t -> point

  val equation : t -> arith -> point

  (** returns the square size of a segment *)
  val sq_size : t -> arith

  (** returns the size of a segment *)
  val size : t -> arith

  val scale_x : t -> arith -> t

  val scale_y : t -> arith -> t

  val translate : arith -> arith -> t -> t

  val transform : affine -> t -> t

  val to_line : t -> L.t

  val contains : t -> point -> bool

  val proj_x : t -> arith * arith

  val proj_y : t -> arith * arith

  val intersects : t -> t -> bool
  (** intersects a b, returns true if a and b intersect. false otherwise*)

  val intersection : t -> t -> point option
  (** returns the intersection point of two segments.
    returns None if they don't intersect*)

  val intersect_line : t -> L.t -> point option
(** returns the intersection point of a segment and a line.
    returns None if they don't intersect*)

end
