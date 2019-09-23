module type Affine_Sig = sig
  type arith
  type t
  val identity : t
  val translation : arith -> arith -> t
  val translate : arith -> arith -> t -> t
  val scaling : arith -> t
  val scale : arith -> t -> t
  val rotation : arith -> t
  val rotate : arith -> t -> t
  val apply : t -> t -> t
  val det : t -> arith
  val transform_distance : t -> arith -> arith -> arith * arith
  val transform_point : t -> arith -> arith -> arith * arith
end

module type Point_Sig = sig
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

module type Line_sig = sig
  type arith
  type point
  type t = X of arith | Y of arith * arith
  val print : Format.formatter -> t -> unit
  type error = Parallel of t * t | Same_coordinates of point
  exception Error of error
  val print_error : Format.formatter -> error -> unit
  val make : arith -> arith -> arith -> t
  val make_x : arith -> t
  val make_y : arith -> arith -> t
  val x_axis : t
  val y_axis : t
  val is_vertical : t -> bool
  val is_horizontal : t -> bool
  val get_coeff : t -> arith * arith * arith
  val to_string : t -> string
  val of_points : point -> point -> t
  val x_from_y : t -> arith -> arith
  val y_from_x : t -> arith -> arith
  val contains : t -> point -> bool
  val scale_x : t -> arith -> t
  val scale_y : t -> arith -> t
  val translate : arith -> arith -> t -> t
  val parallel : t -> t -> bool
  val intersects : t -> t -> bool
  val intersection : t -> t -> point
  val perpendicular : t -> t -> bool
  val perpendicular_of_line : t -> point -> t
  val parallel_of_line : t -> point -> t
  val orth_proj : t -> point -> point
  val point_bissection : point -> point -> t
  val arbitrary_point : t -> point
end

(** Segment manipulation *)
module type Segment_sig = sig
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
  val to_line : t -> line
  val contains : t -> point -> bool
  val proj_x : t -> arith * arith
  val proj_y : t -> arith * arith

  (** intersects a b, returns true if and only if a and b intersect *)
  val intersects : t -> t -> bool

  (** returns the intersection point of two segments.  returns None if
     they don't intersect*)
  val intersection : t -> t -> point option

  (** returns the intersection point of a segment and a line.  returns
   None if they don't intersect*)
  val intersect_line : t -> line -> point option

end

module type Vector_sig = sig
  type arith
  type point
  type t = {dx : arith; dy : arith}

  val print : Format.formatter -> t -> unit
  val make : arith -> arith -> t
  val null : t
  val x_coord : t -> arith
  val y_coord : t -> arith
  val of_points : point -> point -> t
  val magnitude : t -> arith
  val magnitude_sq : t -> arith
  val normalize : t -> t
  val rotation : arith -> t -> t
  val dot_product : t -> t -> arith
  val determinant : t -> t -> arith
  val scal_mult : arith -> t -> t
  val opposite : t -> t
  val add : t -> t -> t
  val substract : t -> t -> t
  val move_to : t -> point -> point
  val projection : t -> t -> t
  val angle : t -> t -> arith
  val angle_deg : t -> t -> arith
  val reflect : t -> t -> t
end

module type Rectangle_sig = sig
  type arith
  type segment
  type point
  type line
  type t

  val make : point -> arith -> arith -> t
  val of_diagonal : point -> point -> t
  val bottom_left_corner : t -> 'a
  val bottom_right_corner : t -> point
  val top_right_corner : t -> point
  val top_left_corner : t -> point
  val scale_x : t -> arith -> t
  val scale_y : t -> arith -> t
  val scale : t -> arith -> t
  val translate : arith -> arith -> t -> t
  val point_reflection : point -> t -> t
  val contains : t -> point -> bool
  val area : t -> arith
  val perimeter : t -> arith
  val proj_x : t -> arith * arith
  val proj_y : t -> arith * arith
  val intersects : t -> t -> bool
  val segments : t -> segment list
  val is_square : t -> bool
  val encompass : t -> point -> t
  val bounding : point list -> t
  val intersect_line : t -> line -> point list
  val centroid : t -> point
  val random_point : t -> point
  val print : Format.formatter -> t -> unit
end


module type Triangle_sig = sig

  type arith
  type point
  type line
  type segment
  type affine

  (** the type of triangles *)
  type t = point * point * point

  (** tri_map f t applies function f in turn to all the points of t and
    stores the results in a new triangle that is returned. *)
  val tri_map : (point -> point) -> t -> t

  (** Higher order utilities over the triangles *)
  val tri_exists : (point -> bool) -> t -> bool
  val tri_find : (point -> bool) -> t -> point
  val tri_forall : (point -> bool) -> t -> bool
  val tri_iter : (point -> unit) -> t -> unit

  (** affine transformation of a triangle *)
  val transform : (t -> affine -> t)

  (** builds a triangle from three different points.
raises Invalid_arg if at least two points are equal*)
  val make : point -> point -> point -> t

  (** returns an arbitrary vertice of the triangle.
The vertice returned is different than the one returned by extr2 and extr3.*)
  val extr1 : t -> point

  (** returns an arbitrary vertice of the triangle.
The vertice returned is different than the one returned by extr1 and extr3.*)
  val extr2 : t -> point

  (** returns an arbitrary vertice of the triangle.
The vertice returned is different than the one returned by extr1 and extr2.*)
  val extr3 : t -> point

  val scale_x : t -> arith -> t
  val scale_y : t -> arith -> t

  val translate : arith -> arith -> t -> t
  val point_reflection :  point -> t -> t

  (** tests if a point is in a triangle *)
  val contains : t -> point -> bool

  val area : t -> arith
  val perimeter : t -> arith
  val proj_x : t -> arith * arith
  val proj_y : t -> arith * arith
  val segments : t -> segment * segment * segment
  val intersects : t -> t -> bool
  val intersect_line : t -> line -> point list
  val is_isoscele : t -> bool
  val is_equilateral : t -> bool
  val is_right : t -> bool
  val points : t -> point * point * point
  val as_points : point * point * point -> t
  val angles : t -> arith * arith * arith

  (** returns the gravity center of a triangle *)
  val centroid : t -> point

  (** returns a randomly and uniformly chosen point of the triangle *)
  val random_point : t -> point

  (** printer *)
  val print : Format.formatter -> t -> unit
end
