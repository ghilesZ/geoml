open Geom
open Utils

module Rand = struct

  let size_x = 800.
  and size_y = 700.
  and title = "test of random distribution"

  type t = Circle.t * Triangle.t * Rectangle.t

  let work (c,t,r) =
    (list_make (fun () -> Circle.random_point c) 1000),
    (list_make (fun () -> Triangle.random_point t) 1000),
    (list_make (fun () -> Rectangle.random_point r) 1000)
      
  let new_val () =
    let rect = Rectangle.make (Point.make 200. 200.) (size_x-.200.) (size_y -.200.) in
    let c = Circle.make (Rectangle.random_point rect) 200.
    and tri = gen_triangle 0. 800. 0. 700.
    and r = Rectangle.make (Rectangle.random_point rect) 200. 300. in
    c,tri,r

  let frame (c,tri,rect) =
    Drawing.draw_string 25 675 "Press 'r' to generate new shapes" Graphics.black;
    let pts1,pts2,pts3 = work (c,tri,rect) in  
    List.iter (fun e -> Drawing.draw_point e Graphics.green) pts1;
    List.iter (fun e -> Drawing.draw_point e Graphics.red) pts2;
    List.iter (fun e -> Drawing.draw_point e Graphics.magenta) pts3;
    Drawing.draw_circle c Graphics.blue;
    Drawing.draw_triangle tri Graphics.blue;
    Drawing.draw_rectangle rect Graphics.blue

end
module Go = Tester.Make(Rand)
