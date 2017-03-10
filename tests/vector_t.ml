open Geom
open Utils

module Vect = struct

  let size_x = 800.
  and size_y = 700.
  and padding = 10.
  and title = "test of random distribution"

  type t = Point.t list

  let new_val () =
    list_make (fun () ->
        gen_point padding (size_x/.2.-.padding) padding (size_y-.padding)
      ) 10

  let frame (v_l : Point.t list) =
    Drawing.draw_string 25 675 "Press 'r' to generate new vectors" Graphics.black;
    let center = Point.make (size_x /. 2.)  (size_y /. 2.) in
    List.iter (fun e -> Drawing.draw_segment (Segment.make e center) Graphics.green) v_l;
    let reflected =
      List.map (fun p ->
          let v = Vector.of_points p center in
          let v' = Vector.reflect v (Vector.make 1. 1.) in
          Vector.move_to v' center
        ) v_l in
    List.iter (fun e -> Drawing.draw_segment (Segment.make e center) Graphics.red) reflected
end

module Go = Tester.Make(Vect)
