open Geom
open Utils

module Circ = struct

  let size_x = 800.
  and size_y = 700.
  and title = "Calculating the circumscribed circle and the incircle of a triangle"

  type t = Triangle.t

  let new_val () = gen_triangle 200. (size_x-.200.) 200. (size_y-.200.)

  let work t =
    let (pa,pb,pc) = Triangle.points t in
    let inc = (Circle.incircle pa pb pc) in
    (Circle.circumscribed pa pb pc), inc

  let frame v =
    Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;
    Drawing.draw_triangle ~lw:1 v Graphics.blue;
    Triangle.tri_iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c Graphics.green
    ) v;
    let (c1,c2) = (work v) in
    Drawing.draw_circle c1 Graphics.red;
    Drawing.draw_circle c2 Graphics.blue

end
module Go = Tester.Make(Circ)
