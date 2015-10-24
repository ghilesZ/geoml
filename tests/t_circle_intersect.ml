open Geom
open Utils

module Poly_r = struct
  let size_x = 800.
  and size_y = 700.
  and title = "Random regular polygon"

  type t = Polygon.t * Polygon.Regular.t

  let new_val () =
    let rp = (gen_regular 200. (size_x-.200.) 200. (size_y-.200.)) in
    let p = Polygon.Regular.to_randomized_polygon rp in
    p, rp


  let frame (p1, p2) =
    Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;
    Drawing.draw_polygon p1 Graphics.green;
    Drawing.draw_regular p2 Graphics.blue

end
module Go = Tester.Make(Poly_r)
let _ =  Go.doit()
