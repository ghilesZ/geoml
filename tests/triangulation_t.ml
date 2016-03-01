

open Geom
open Utils

module Triangulation = struct
  let size_x = 800.
  and size_y = 700.
  and title = "Random regular polygon"

  type t = Polygon.t * (Point.t * Point.t * Point.t) list

  let (!%) f = fun a b -> f b a

  let new_val () =
    let rp = (gen_regular 200. (size_x-.200.) 200. (size_y-.200.)) in
    let p1 = Polygon.Regular.to_randomized_polygon rp in
    p1, Polygon.triangulation p1

  let frame (p1, triangles) =
    Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;

    List.iter (fun (v1, v2, v3) ->
        Drawing.draw_triangle (Triangle.make v1 v2 v3) Graphics.red
      ) triangles;



end
module Go = Tester.Make(Triangulation)
