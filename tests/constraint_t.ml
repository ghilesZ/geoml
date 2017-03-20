open Geom
open Utils

module Half = struct
  let size_x = 800.
  and size_y = 600.
  and title = "constraint redundancy checking"

  let padding = 40.

  type t = Point.t list * Point.t list

  let new_val () : t =
    let c1 = Circle.make (Point.make (0.6*.size_x) (size_y/.2.)) 200.
    and c2 = Circle.make (Point.make (0.4*.size_x) (size_y/.2.)) 200.
    in
    ((list_make (fun _ -> Circle.random_point_perimeter c1) 1000),
    (list_make (fun _ -> Circle.random_point_perimeter c2) 1000))

  let frame (l1,l2:t) =
    let p1 = Polygon.bounding l1 and p2 = Polygon.bounding l2 in
    let p1' = Polyhedron.of_polygon p1
    and p2' = Polyhedron.of_polygon p2 in
    let p3 = Polyhedron.intersection p1' p2' in
    let size_p3 = List.length (Polyhedron.get_constr p3) in
    Format.printf "%i constraints in intersection before removal\n" (size_p3);
    let p3' = Polyhedron.remove_redundancies p3 in
    let size_p3' = List.length (Polyhedron.get_constr p3') in
    Format.printf "%i constraints in intersection after removal\n%!" (size_p3');
    Drawing.draw_polygon ~lw:3 p1 Graphics.blue;
    Drawing.draw_polygon ~lw:3 p2 Graphics.green;
    List.iter (fun c -> Drawing.draw_line (Constraint.get_border c) Graphics.red)
              (Polyhedron.get_constr p3');
    Drawing.draw_string 25 585 "Press 'R' to refresh" Graphics.black

end
module Go = Tester.Make(Half)
