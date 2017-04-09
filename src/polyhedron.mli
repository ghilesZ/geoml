(**
    The module for the polyhedron manipulation.
    We use the term "polyhedron" to define a set of linear constraint.
    Each constraint defines a half-plan. A polyhedron is then determined by the intersections of all
    the half-plans formed by its constaint.
    It differs from polygons which represent finite enveloppes (For example, convex polygons are a
    particular cases of polyhedra, which can be infinite).
*)

type t = private Constraint.t list

val make : Constraint.t list -> t

(** checks if the space defined by the constraint list is empty *)
val is_empty : t -> bool

(** checks if the space defined by the constraint list is finite *)
val is_closed : t -> bool

(** checks if the space defined by the constraint list is infinite *)
val is_open : t -> bool

val contains : t -> Point.t -> bool

val translate : float -> float -> t -> t

val intersection : t -> t -> t

(** remove redundant constraints of a polyhedra *)
val remove_redundancies : t -> t

(** returns the polyhedron corresponding to the space defined by a polygon *)
val of_polygon : Polygon.Convex.t -> t

(** returns the polygon corresponding to the space defined by a polyhedron.
    raises a failure if the polyhedron is open *)
val to_polygon : t -> Polygon.Convex.t

(** returns the list of constraint of the polyhedron *)
val get_constr : t -> Constraint.t list
