open Geoml

let iof = int_of_float

let foi = float_of_int

let list_make f s =
  let rec aux res = function 0 -> res | n -> aux (f () :: res) (n - 1) in
  aux [] s

(*************** Random utilities *******************)
let gen_point xmin xmax ymin ymax =
  let x = xmin +. foi (Random.int (int_of_float (xmax -. xmin)))
  and y = ymin +. foi (Random.int (int_of_float (ymax -. ymin))) in
  Point.make x y

let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax in
  Triangle.make p1 p2 p3

(****************** Main example **********************)

let size_x = 800.

and size_y = 700.

and title = "test of random distribution"

let work (c, t, r) =
  let st = Random.get_state () in
  ( list_make (fun () -> Circle.random_point st c) 1000
  , list_make (fun () -> Triangle.random_point st t) 1000
  , list_make (fun () -> Rectangle.random_point st r) 1000 )

let new_val () =
  let st = Random.get_state () in
  let rect =
    Rectangle.make (Point.make 200. 200.) (size_x -. 200.) (size_y -. 200.)
  in
  let c = Circle.make (Rectangle.random_point st rect) 200.
  and tri = gen_triangle 0. 800. 0. 700.
  and r = Rectangle.make (Rectangle.random_point st rect) 200. 300. in
  (c, tri, r)

let frame (c, tri, rect) =
  Drawing.draw_string 25 675 "Press 'r' to generate new shapes"
    Graphics.black ;
  let pts1, pts2, pts3 = work (c, tri, rect) in
  List.iter (fun e -> Drawing.draw_point e Graphics.green) pts1 ;
  List.iter (fun e -> Drawing.draw_point e Graphics.red) pts2 ;
  List.iter (fun e -> Drawing.draw_point e Graphics.magenta) pts3 ;
  Drawing.draw_circle c Graphics.blue ;
  Drawing.draw_triangle tri Graphics.blue ;
  Drawing.draw_rectangle rect Graphics.blue

(****** Window and event handling *****)
let clear () = Drawing.fill_screen Graphics.white

let handler status =
  let open Graphics in
  if status.key = 'r' then (
    clear () ;
    new_val () |> frame )

let loop () = Graphics.loop_at_exit [Graphics.Key_pressed] handler

let doit () =
  Random.self_init () ;
  Drawing.open_graph size_x size_y title ;
  new_val () |> frame ;
  loop ()

let () = doit ()
