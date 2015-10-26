open Geom
open Utils

module T = struct
  let size_x = 800. 
  and size_y = 600.
  and size = 15
  and title = "Calculating the bounding circle/rectangle/polygon of a point list"

  type t = Point.t list  
    
  let new_val () = list_make
    (fun _ -> gen_point 30. (size_x-.30.) 30. (size_y-.30.))
    size 
      
  let frame v =
    Drawing.draw_string 25 25 "Press 'r' to generate a new cloud" Graphics.black;
    List.iter (fun e -> 
      let c = Circle.make e 5. in 
      Drawing.fill_circle c Graphics.black
    ) v;
    Drawing.draw_rectangle (Rectangle.bounding v) Graphics.green;
    Drawing.draw_polygon (Polygon.bounding v) Graphics.blue;
    let c = Circle.bounding v in
    Drawing.draw_circle c Graphics.red
end

module Go = Tester.Make(T)

let _ = Go.doit()
