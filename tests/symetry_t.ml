open Geom
open Utils

module Sym = struct

  let size_x = 800.
  and size_y = 700.
  and padding = 50.
  and title = "Symetry tests"

  type t = Rectangle.t * Triangle.t * Circle.t * Point.t
    
  let new_val () =
    let p = Point.make (size_x/.2.) (size_y/.2.) in
    let size_x = size_x /. 2. in
    let c = gen_point padding (size_x -. padding) padding (size_y -. padding)
    and tr = gen_triangle padding (size_x -. padding) padding (size_y -. padding)
    and p2 = gen_point padding (size_x -. padding) padding (size_y -. padding) in
    let r = Rectangle.make p2 (100.) (60.) 
    and c = Circle.make c 50. in
    (r,tr,c,p)
    
  let frame (r,tr,c,p) = 
    Drawing.draw_rectangle r Graphics.blue;
    Drawing.draw_rectangle (Rectangle.point_reflection p r) Graphics.red;
    Drawing.draw_triangle tr Graphics.blue;
    Drawing.draw_triangle (Triangle.point_reflection p tr) Graphics.red;
    Drawing.draw_circle c Graphics.blue;
    Drawing.draw_circle (Circle.point_reflection p c) Graphics.red;
    let c = Circle.make p 5. in 
    Drawing.fill_circle c (Graphics.rgb 255 0 255)

end

module Go = Tester.Make(Sym)

