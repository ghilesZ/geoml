open Geom
open Utils

module T = struct
  let size_x = 1800. 
  and size_y = 1000.
  and size = 5000000
  and padding = 30.
  and title = "Calculating the bounding circle/rectangle/polygon of a point list"

  type t = Point.t list  
    
  let new_val () = list_make
    (fun _ -> gen_point padding (size_x-.padding) padding (size_y-.padding))
    size 
      
  let frame v =
    Drawing.draw_string 25 25 "Press 'r' to generate a new cloud" Graphics.black;
    (*Drawing.draw_rectangle (Rectangle.bounding v) Graphics.green;
    Drawing.draw_polygon (Polygon.bounding v) Graphics.blue;*)
    List.iter (fun e -> 
      let c = Circle.make e 1. in 
      Drawing.fill_circle c Graphics.black
    ) v;
    let c = Circle.bounding v in
    Drawing.draw_circle c Graphics.red
end

module Go = Tester.Make(T)

let _ = Go.doit()
