open Geoml

let iof = int_of_float
let foi = float_of_int

let list_make f s =
  let rec aux res = function
  | 0 -> res
  | n -> aux ((f ())::res) (n-1)
  in aux [] s

(*************** Random utilities *******************)
let gen_point xmin xmax ymin ymax =
  let x = xmin +. (foi (Random.int (iof (xmax-.xmin))))
  and y = ymin +. (foi (Random.int (iof (ymax-.ymin)))) in
  Point.make x y

(****************** Main example **********************)
let size_x = 800.
and size_y = 700.
and padding = 50.
and title = "BSpline curves"

let new_val () =
  let tmp = ref padding in
  let next split () =
    let res = !tmp in
    tmp:=!tmp +. split;
    gen_point res (res +. split) padding (size_y-.padding)
  in
  let nb = 20 in
  let split = (size_x -. 2. *.padding) /. (float_of_int nb) in
  list_make (next split) nb

let frame pts =
  Drawing.draw_string 25 675 "Press 'r' to generate new curves" Graphics.black;
  List.fold_left (fun a b ->
      Drawing.draw_segment (Segment.make a b) Graphics.green;
      b
    ) (List.hd pts) (List.tl pts) |> ignore;
  let quads = Curve.Quadratic.of_points pts in
  List.iter (fun c -> Drawing.draw_quadratic_curve c Graphics.yellow) quads;
  let cubs = Curve.Cubic.of_points pts in
  List.iter (fun c -> Drawing.draw_cubic_curve c Graphics.red) cubs

(****** Window and event handling *****)
let clear () = Drawing.fill_screen Graphics.white

let handler status =
  let open Graphics in
  if status.key = 'r' then begin
      clear ();
      new_val () |> frame
    end

let loop () =
  Graphics.loop_at_exit [Graphics.Key_pressed] handler

let doit () =
  Random.self_init ();
  Drawing.open_graph size_x size_y title;
  new_val () |> frame;
  loop ()

let () = doit()
