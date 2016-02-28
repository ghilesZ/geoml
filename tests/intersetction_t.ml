open Geom
open Utils

module Inter = struct

  let size_x = 800.
  and size_y = 700.
  and padding = 10.
  and size = 2
  and title = "Calculating the intersection points of a circle and a line"
    
  type t = Circle.t * Circle.t * Line.t 

  let new_val () =
    let a = gen_point padding (size_x-.padding) padding (size_y-.padding)
    and b = gen_point padding (size_x-.padding) padding (size_y-.padding)
    and c = gen_point padding (size_x-.padding) padding (size_y-.padding)
    and d = gen_point padding (size_x-.padding) padding (size_y-.padding)
    and radius = 100. in
    let l = Line.of_points a b
    and c1 = Circle.make c radius
    and c2 = Circle.make d radius
    in c1,c2,l

  let work (c1,c2,l) = 
    let l1 = Circle.line_intersection c1 l
    and l2 = Circle.line_intersection c2 l
    and l3 = Circle.intersection c1 c2
    in List.concat [l1;l2;l3]

  let work2 c = 
    let top = Line.make_y 0. size_y
    and right = Line.make_x size_x
    in 
    let a = Circle.line_intersection c Line.x_axis
    and b = Circle.line_intersection c Line.y_axis
    and c = Circle.line_intersection c top
    and d = Circle.line_intersection c right
    in
    List.concat [a;b;c;d]

  let frame ((c1,c2,l):t) =
    Drawing.draw_string 25 675 "Press 'r' to generate a circle and a new line" Graphics.black;
    Drawing.draw_circle c1 Graphics.red;
    Drawing.draw_circle c2 Graphics.red;
    Drawing.draw_line l Graphics.blue;
    let pts = work (c1,c2,l) in
    List.iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c (Graphics.rgb 200 100 200))
      pts;
    let pts = work2 c1 in
    List.iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c (Graphics.rgb 255 165 0))
      pts;
    let pts = work2 c2 in
    List.iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c (Graphics.rgb 255 165 0))
      pts
end

module Go = Tester.Make(Inter)
let _ = Go.doit()
