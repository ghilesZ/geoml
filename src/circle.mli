(** Circle manipulation *)
module Make: functor
  (A:Arith.T)
  (P:Signatures.Point_Sig   with type arith = A.t)
  (L:Signatures.Line_sig    with type point = P.t and type arith = A.t)
  (S:Signatures.Segment_sig with type point = P.t and type line = L.t) -> sig

  type arith
  type point
  type line
  type segment

  type t = {
      center:P.t;
      radius:arith;
    }

  val make : point -> arith -> t

  val center : t -> point

  val radius : t -> arith

  val translate : arith -> arith -> t -> t

  val point_reflection : point -> t -> t

  (** radian rotation. rotate c p f returns the rotated circle of c with p as
    rotation center and f a angle in radian *)
  val rotate : t -> point -> arith -> t

  (** degree rotation. rotate c p f returns the rotated circle of c with p as
    rotation center and f a angle in degree *)
  val rotate_angle : t -> point -> arith -> t

  val contains : t -> point -> bool

  val area : t -> arith

  val perimeter : t -> arith

  val proj_x : t -> arith * arith

  val proj_y : t -> arith * arith

  (** tangent c p returns the tangent of circle c going through point p.
    p must lie on c's boundary, otherwise behaviour is unspecified *)
  val tangent : t -> point -> line

  val intersects : t -> t -> bool

  val intersection : t -> t -> point list
  (** returns the list of intersection points of two circle. It can be:
    - [] when the circles dont intersect
    - [p] when the circles are tangent in p
    - [a;b] when the circles intersect, a and b are the intersection points*)

  val intersect_line : t -> L.t -> point list
  (** same as intersection but with a circle and a line *)

  val segment_intersection : t -> S.t -> point list
  (** same as intersection but with a circle and a segment *)

  val circumscribed : point -> point -> point -> t
  (** returns the circumscribed cirle of the triangle defined by the three points*)

  val incircle : point -> point -> point -> t
  (** returns the incirle of the triangle defined by the three points*)

  val of_diameter : point -> point -> t
  (** of_diameter a b builds the circle with ab as diameter*)

  val bounding : point list -> t
  (** given a list of point, returns the smallest circle that
   contains all the points of the list, using emo welzl's algorithm.
    complexity in expected linear time *)

  val random_point : t -> point
  (** returns a randomly and uniformly chosen point that lies inside the circle *)

  val random_point_perimeter : t -> point
  (** returns a randomly and uniformly chosen point that lies on the permieter of the circle *)

  val print : Format.formatter -> t -> unit
                                         (** printer *)

end
