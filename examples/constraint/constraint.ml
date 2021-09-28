open Geoml

let iof = int_of_float

let foi = float_of_int

let list_make f s =
  let rec aux res = function 0 -> res | n -> aux (f () :: res) (n - 1) in
  aux [] s

(*************** Random utilities *******************)
let gen_point xmin xmax ymin ymax =
  let x = xmin +. foi (Random.int (iof (xmax -. xmin)))
  and y = ymin +. foi (Random.int (iof (ymax -. ymin))) in
  Point.make x y

let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax in
  Triangle.make p1 p2 p3

let size_x = 800.

and size_y = 600.

and title = "constraint redundancy checking"

let padding = 40.

let new_val () =
  let st = Random.get_state () in
  let c1 = Circle.make (Point.make (0.6 *. size_x) (size_y /. 2.)) 200.
  and c2 = Circle.make (Point.make (0.4 *. size_x) (size_y /. 2.)) 150. in
  ( list_make (fun _ -> Circle.random_point_perimeter st c1) 100
  , list_make (fun _ -> Circle.random_point_perimeter st c2) 100 )

let frame (l1, l2) =
  let p1 = Polygon.Convex.hull l1 and p2 = Polygon.Convex.hull l2 in
  let p1' = Polyhedron.of_polygon p1 and p2' = Polyhedron.of_polygon p2 in
  let p3 = Polyhedron.intersection p1' p2' in
  let size_p3 = List.length (Polyhedron.get_constr p3) in
  Format.printf "%i constraints in intersection before removal\n" size_p3 ;
  let p3' = Polyhedron.remove_redundancies p3 in
  let size_p3' = List.length (Polyhedron.get_constr p3') in
  Format.printf "%i constraints in intersection after removal\n%!" size_p3' ;
  Drawing.draw_convex_polygon ~lw:3 p1 Graphics.blue ;
  Drawing.draw_convex_polygon ~lw:3 p2 Graphics.green ;
  List.iter
    (fun c -> Drawing.draw_line (Constraint.get_border c) Graphics.red)
    (Polyhedron.get_constr p3') ;
  Drawing.draw_string 25 585 "Press 'R' to refresh" Graphics.black

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
