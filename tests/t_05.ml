open Geom
open Utils

module Inter = struct

  let size_x = 800.
  and size_y = 700.
  and title = "Calculating the intersection points of a circle and a line"
    
  type t = Circle.t*Line.t 

  let new_val () =
    let a = gen_point 200. (size_x-.200.) 200. (size_y-.200.)
    and b = gen_point 200. (size_x-.200.) 200. (size_y-.200.)
    and c = gen_point 200. (size_x-.200.) 200. (size_y-.200.)
    and radius = 100. in
    let l = Line.of_points a b
    and c = Circle.make c radius
    in c,l
    
  let work (c,l) = Circle.intersection c l

  let frame (c,l) =
    Drawing.draw_string 25 675 "Press 'r' to generate a circle and a new line" Graphics.black;
    Drawing.draw_circle c Graphics.red;
    Drawing.draw_line l Graphics.blue;
    let pts = work (c,l) in
    List.iter (fun e ->
      (*Format.printf "%a" Point.print e;print_newline ();*)
      let c = Circle.make e 5. in
      Drawing.fill_circle c (Graphics.rgb 255 128 255))
      pts
    

end

module Go = Tester.Make(Inter)
let _ = Go.doit()
