(** Circle manipulation *)

type t = private Point.t * float

val make : Point.t -> float -> t
  
val center : t -> Point.t

val radius : t -> float

val translate : float -> float -> t -> t

val point_reflection : Point.t -> t -> t

(** radian rotation. rotate c p f returns the rotated circle of c with p as
    rotation center and f a angle in radian *)
val rotate : t -> Point.t -> float -> t

(** degree rotation. rotate c p f returns the rotated circle of c with p as
    rotation center and f a angle in degree *)
val rotate_angle : t -> Point.t -> float -> t

val print : Format.formatter -> t -> unit
val contains : t -> Point.t -> bool

val area : t -> float

val perimeter : t -> float

val proj_x : t -> float * float

val proj_y : t -> float * float

val intersects : t -> t -> bool

val intersection : t -> t -> Point.t list
(** returns the list of intersection points of two circle. It can be:
    - [] when the circles dont intersect
    - [p] when the circles are tangent in p
    - [a;b] when the circles intersect, a and b are the intersection points*)

val line_intersection : t -> Line.t -> Point.t list
(** same as intersection but with a circle and a line *)
 
val segment_intersection : t -> Segment.t -> Point.t list
(** same as intersection but with a circle and a segment *)
    
val circumscribed : Point.t -> Point.t -> Point.t -> t
(** returns the circumscribed cirle of the triangle defined by the three points*)  
  
val incircle : Point.t -> Point.t -> Point.t -> t
(** returns the incirle of the triangle defined by the three points*)  
  
val of_diameter : Point.t -> Point.t -> t
(** of_diameter a b builds the circle with ab as diameter*)

val bounding : Point.t list -> t
(** given a list of point, returns the smallest circle that
   contains all the points of the list, using emo welzl's algorithm.
    complexity in expected linear time *)

val print : Format.formatter -> t -> unit  
(** printer *)

