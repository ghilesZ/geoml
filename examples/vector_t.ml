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
        gen_point padding (size_x/.2.-.padding) padding (size_y/.2.-.padding)
      ) 100

  let frame (v_l : Point.t list) =
    Drawing.draw_string 25 675 "Press 'r' to generate new vectors" Graphics.black;
    let center = Point.make (size_x /. 2.)  (size_y /. 2.) in
    let axis = Vector.make 0. 1. in
    let v0 = List.map (fun p -> Vector.of_points p center) v_l in
    let v1 = List.map (fun v -> Vector.reflect v axis) v0 in
    let v2 = List.map Vector.opposite v1 in
    let v3 = List.map (fun v -> Vector.reflect v axis) v2 in
    let draw_seg col = List.iter (fun v ->
                       let s = Segment.make (Vector.move_to v center) center in
                       Drawing.draw_segment s col)
    in
    draw_seg Graphics.red v0;
    draw_seg Graphics.green v1;
    draw_seg Graphics.blue v2;
    draw_seg Graphics.cyan v3
end

module Go = Tester.Make(Vect)
