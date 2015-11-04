open Geom
open Utils

module Circ = struct

  let size_x = 800.
  and size_y = 700.
  and padding = 50.
  and title = "BSpline curves"

  type t = Point.t list
    
  let new_val () =
    let tmp = ref padding in
    let next split () =
      let res = !tmp in
      tmp:=!tmp +. split;
      gen_point res (res +. split) padding (size_y-.padding)
    in
    let nb = 10 in
    let split = (size_x -. 2. *.padding) /. (float_of_int nb) in
    list_make (next split) nb

  let frame pts =
    Drawing.draw_string 25 675 "Press 'r' to generate new curves" Graphics.black;
    List.fold_left (fun a b ->
      Drawing.draw_segment (Segment.make a b) Graphics.green;
      b
    ) (List.hd pts) (List.tl pts) |> ignore;
    let bs = Curve.BSpline.make_eq pts 20 in
    Drawing.draw_bspline bs Graphics.red
      
end

module Go = Tester.Make(Circ)

let _ =  Go.doit()
