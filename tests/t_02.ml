open Utils

module Circ = struct

  let size_x = 800.
  and size_y = 700.
  and title = "Calculating the circumscribed circle of a triangle"

  type t = Triangle.t  

  let new_val () = gen_triangle 200. (size_x-.200.) 200. (size_y-.200.)

  let circumscribed t = 
    let (pa,pb,pc) = Triangle.points t in
    Circle.circumscribed pa pb pc

  let frame v =
    Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;
    Drawing.draw_triangle ~lw:2 v Graphics.blue;
    Triangle.tri_iter (fun e -> 
      let c = Circle.make e 5. in
      Drawing.fill_circle c Graphics.green
    ) (Triangle.points v );
    Drawing.draw_circle (circumscribed v) Graphics.red

end
module Go = Tester.Make(Circ)  
let _ =  Go.doit()
