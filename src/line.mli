(** This module provides basic operation over the linear equation type *)

type t = private 
	 | X of float
	 | Y of float * float
(** the linear equation type *)

val print : Format.formatter -> t -> unit
(** printer *)

val to_string : t -> string
(** to string*)

type error = | Parallel of t * t
	     | Same_coordinates of Point.t
(** the type of errors *)

exception Error of error
(** the type of exceptions concerning this module *)
    
val print_error : Format.formatter -> error -> unit
(** error printer*)
  
val make : float -> float -> float -> t
(** make a b c returns a line with an equation of the form: ax + by + c = 0*)
  
val make_x : float -> t
(** make_x a returns a line with an equation of the form: x = a*)
  
val make_y : float -> float -> t
(** make_y a b returns a line with an equation of the form: y = ax + b *)  

val x_axis : t
(** returns the horizontal line of equation: y = 0*)
  
val y_axis : t
(** returns the vertical line of equation: x = 0*)

val of_points : Point.t -> Point.t -> t    
(** of_points p1 p2 builds the line that goes through p1 and p2. 
    It raises Error(Same_coordinates) if p1 and p2 have the same coordinates *)

val is_vertical : t -> bool
(** is_vertical l, returns true if l has an equation of the form: x=cst, 
    where cst is a constant float*)
  
val is_horizontal: t -> bool
  (** is_horizontal l, returns true if l has an equation of the form: y=cst, 
      where cst is a constant float*)
  
val get_coeff : t -> (float*float*float)
(** returns a tuple (a,b,c) with respect to the equation of line, as: ax + by + c = 0*)
  
val x_from_y : t -> float -> float
(** 'x_from_y line y', returns 'x', the value on the x-axis corresponding to given 'y' value, 
    with f the affine function associated to 'line', as:
    f(x) = y
    raises Parallel if 'line' doesn't intersect the horizontal line going through 'y'
*)

val y_from_x : t -> float -> float
(** 'y_from_x line x', returns 'y', the value on the y-axis corresponding to given 'x' value, 
    with f the affine function associated to 'line', as:
    f(x) = y 
    raises Parallel if 'line' doesn't intersect the vertical line going through 'x'
*)
  
val contains : t -> Point.t -> bool
(** contains l1 p returns true if l1goes through p. false otherwise.*)

val scale_x : t -> float -> t

val scale_y : t -> float -> t

val translate : t -> float -> float -> t
  
val parallel : t -> t -> bool
(** parallel l1 l2 returns true if l1 and l2 are parallel. false otherwise.*)

val intersects : t -> t -> bool
(** intersects l1 l2 returns true if l1 and l2 intersects. false otherwise.*)
  
val intersection : t -> t -> Point.t
(** intersection l1 l2 returns the point at the intersection of l1 and l2.
    It raises Error(Parralel) if l1 and l2 dont intersect *)
  
val perpendicular : t -> t -> bool
(** perpendicular l1 l2 returns true if l1 and l2 are perpendicular.
    false otherwise.*)
  
val perpendicular_of_line : t -> Point.t -> t
(** perpendicular_of_line l p  returns the line perpendicular to l that goes through p.*)
  
val parallel_of_line : t -> Point.t -> t
(** parallel_of_line l p  returns the line parallel to l that goes through p.*)

val orth_proj : t -> Point.t -> Point.t
(** orth_proj l p, returns the orthogonal projection of p on l*)
  
val point_bissection : Point.t -> Point.t -> t
(** point_bissection p1 p2 returns the line l that bissects the segment p1p2 in its center*)
