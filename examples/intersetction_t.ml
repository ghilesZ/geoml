open Geom
open Utils

module Inter = struct

  let size_x = 800.
  and size_y = 700.
  and padding = 10.
  and size = 2
  and title = "Calculating the intersection points of two circles and a line"

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
    let l1 = Circle.intersect_line c1 l
    and l2 = Circle.intersect_line c2 l
    and l3 = Circle.intersection c1 c2
    in List.concat [l1;l2;l3]

  let work2 c =
    let top = Line.make_y 0. size_y
    and right = Line.make_x size_x
    in
    let a = Circle.intersect_line c Line.x_axis
    and b = Circle.intersect_line c Line.y_axis
    and c = Circle.intersect_line c top
    and d = Circle.intersect_line c right
    in
    List.concat [a;b;c;d]

  let frame ((c1,c2,l):t) =
    Drawing.draw_string 25 675 "Press 'r' to generate new circles and a new line" Graphics.black;
    Drawing.draw_circle c1 Graphics.red;
    Drawing.draw_circle c2 Graphics.red;
    Drawing.draw_line l Graphics.blue;
    List.iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c (Graphics.rgb 200 100 200))
      (work (c1,c2,l));
    List.iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c (Graphics.rgb 255 165 0))
      (work2 c1);
    List.iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c (Graphics.rgb 255 165 0))
      (work2 c2)
end

module Go = Tester.Make(Inter)
