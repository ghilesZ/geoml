open Geom
open Utils

module Half = struct
  let size_x = 800.
  and size_y = 600.
  and title = "polyhedra simple tests"

  let padding = 40.

  type t = Polyhedron.t

  let new_val () : t =
    let comp() =
      Constraint.(match Random.int 4 with
      | 0 -> Lt
      | 1 -> Gt
      | 2 -> Geq
      | _ -> Leq)
    in
    let cons() =
      let point() = gen_point padding (size_x-.padding) padding (size_y-.padding) in
      let line = Line.of_points (point()) (point()) in
      Drawing.draw_line line Graphics.blue;
      Constraint.(make line (comp()))
    in
    Polyhedron.make (list_make (fun _ -> cons()) 3)

  let frame (p:t) =
    Drawing.fill_polyhedron p Graphics.red;
    Drawing.draw_string 25 585 "Press 'R' to refresh" Graphics.black

end
module Go = Tester.Make(Half)
