

open Geom
open Utils

module Poly_r = struct
  let size_x = 800.
  and size_y = 700.
  and title = "Random regular polygon"

  type t = Polygon.t * Polygon.t

  let (!%) f = fun a b -> f b a

  let new_val () =
    let rp = (gen_regular 200. (size_x-.200.) 200. (size_y-.200.)) in
    let p1 = Polygon.Regular.to_randomized_polygon rp in
    let p2 = Polygon.transform Affine.(scaling 0.5) p1 in
    p1, p2

  let frame (p1, p2) =
    Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;

    Graphics.set_line_width 1;
    Drawing.draw_polygon p1 Graphics.green;
    Drawing.draw_polygon p2 Graphics.blue


end
module Go = Tester.Make(Poly_r)
let _ =  Go.doit()
