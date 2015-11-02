open Geom
open Utils

module Circ = struct

  let size_x = 800.
  and size_y = 700.
  and padding = 100.
  and title = "Quadratic and cubic bezier curves"

  type t = Curve.Quadratic.t * Curve.Quadratic.t * Curve.Quadratic.t * Curve.Cubic.t * Curve.Cubic.t

  let new_val () =
    let split = (size_x-.padding) /. 7 in
    let p1 = gen_point padding (padding +. split) padding (size_y-.padding)
    and p2 = gen_point (padding+.split) (padding +. 2. *. split) padding (size_y-.padding)
    and p3 = gen_point (padding+.2.*.split) (padding+.3.*.split) padding (size_y-.padding)
    and p4 = gen_point (padding+.3.*.split) (padding+.4.*.split) padding (size_y-.padding)
    and p5 = gen_point (padding+.4.*.split) (padding+.5.*.split) padding (size_y-.padding)
    and p6 = gen_point (padding+.5.*.split) (padding+.6.*.split) padding (size_y-.padding)
    and p7 = gen_point (padding+.6.*.split) (padding+.7.*.split) padding (size_y-.padding)
    in
    let c1 = Curve.Quadratic.make p1 p2 p3
    and c2 = Curve.Quadratic.make p3 p4 p5
    and c3 = Curve.Quadratic.make p5 p6 p7
    and c4 = Curve.Cubic.make p1 p2 p3 p4
    and c5 = Curve.Cubic.make p4 p5 p6 p7 in
    (c1,c2,c3,c4,c5)
      
  let frame (c1,c2,c3,c4,c5) =
    Drawing.draw_string 25 675 "Press 'r' to generate new curves" Graphics.black;
        List.iter (fun v -> 
      let c = Circle.make (Curve.Quadratic.start v) 5. in
      Drawing.fill_circle c Graphics.green;
      let c = Circle.make (Curve.Quadratic.ending v) 5. in
      Drawing.fill_circle c Graphics.green;
      let c = Circle.make (Curve.Quadratic.control v) 5. in
      Drawing.fill_circle c Graphics.green
    )
    [c1;c2;c3];
    Drawing.draw_quadratic_curve c1 Graphics.blue;
    Drawing.draw_quadratic_curve c2 Graphics.blue;
    Drawing.draw_quadratic_curve c3 Graphics.blue;
    Drawing.draw_cubic_curve c4 Graphics.red;
    Drawing.draw_cubic_curve c5 Graphics.red

      
end
module Go = Tester.Make(Circ)  
let _ =  Go.doit()
