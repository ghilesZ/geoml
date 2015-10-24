open Geom
open Utils

module T = struct
  let size_x = 800. 
  and size_y = 700. 
  and size = 10
  and title = "Calculating the bounding circle/rectangle/polygon of a point list"

  type t = Point.t list  
    
  let new_val () = list_make
    (fun _ -> gen_point 70. (size_x-.70.) 70. (size_y-.70.))
    size 
      
  let frame v =
    Drawing.draw_string 25 675 "Press 'r' to generate a new cloud" Graphics.black;
    List.iter (fun e -> 
      let c = Circle.make e 5. in 
      Drawing.fill_circle c Graphics.black
    ) v;
    Drawing.draw_circle (Circle.bounding v) Graphics.red;
    Drawing.draw_rectangle (Rectangle.bounding v) Graphics.green;
    Drawing.draw_polygon (Polygon.bounding v) Graphics.blue

end

module Go = Tester.Make(T)

let _ = Go.doit()      
