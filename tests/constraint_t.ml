open Geom
open Utils

module Half = struct
  let size_x = 800.
  and size_y = 600.
  and title = "constraint redundancy checking"

  let padding = 40.

  type t = Point.t list * Point.t list

  let new_val () : t =
    ((list_make (fun _ -> gen_point padding (0.6*.size_x) padding (size_y-.padding)) 30),
    (list_make (fun _ -> gen_point (size_x*.0.4) (size_x-.padding) padding (size_y-.padding)) 30))

  let frame (l1,l2:t) =
    Drawing.draw_string 25 585 "Press 'R' refresh" Graphics.black;
    let p1 = Polygon.bounding l1 and p2 = Polygon.bounding l2 in
    let p1' = Polyhedron.of_polygon p1
    and p2' = Polyhedron.of_polygon p2 in
    let p3 = Polyhedron.remove_redundancies (Polyhedron.intersection p1' p2') in
    Drawing.draw_polygon ~lw:3 p1 Graphics.blue;
    Drawing.draw_polygon ~lw:3 p2 Graphics.green;
    List.iter (fun c -> Drawing.draw_line (Constraint.get_border c) Graphics.red)
              (Polyhedron.get_constr p3)
end
module Go = Tester.Make(Half)
