open Geom
open Utils

module Circ = struct

  let size_x = 800.
  and size_y = 700.
  and padding = 100.
  and title = "Quadratic bezier curve"

  type t = Curve.Quadratic.t 

  let new_val () = 
    let p1 = gen_point padding (size_x-.padding) padding (size_y-.padding)
    and p2 = gen_point padding (size_x-.padding) padding (size_y-.padding)
    and p3 = gen_point padding (size_x-.padding) padding (size_y-.padding)
    in Curve.Quadratic.make p1 p2 p3

  let frame v =
    Drawing.draw_string 25 675 "Press 'r' to generate a new curve" Graphics.black;
    Drawing.draw_quadratic_curve v Graphics.blue;
    let c = Circle.make (Curve.Quadratic.start v) 5. in
    Drawing.fill_circle c Graphics.green;
    let c = Circle.make (Curve.Quadratic.ending v) 5. in
    Drawing.fill_circle c Graphics.green;
    let c = Circle.make (Curve.Quadratic.control v) 5. in
    Drawing.fill_circle c Graphics.red
end
module Go = Tester.Make(Circ)  
let _ =  Go.doit()
