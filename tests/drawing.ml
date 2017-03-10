open Geom
open Graphics

let iof = int_of_float
let soi = string_of_int

let draw_point ?(lw=1) p col =
  set_color col;
  set_line_width lw;
  fill_circle (Point.x_coord p |> iof) (Point.y_coord p |> iof) lw

let draw_circle ?(lw=1) c col =
  set_color col;
  set_line_width lw;
  draw_circle
    (c |> Circle.center |> Point.x_coord |> iof)
    (c |> Circle.center |> Point.y_coord |> iof)
    (c |> Circle.radius |> iof)

let fill_circle ?(lw=1) c col =
  set_color col;
  set_line_width lw;
  fill_circle
    (c |> Circle.center |> Point.x_coord |> iof)
    (c |> Circle.center |> Point.y_coord |> iof)
    (c |> Circle.radius |> iof)

let draw_segment ?(lw=1) s col =
  set_line_width lw;
  set_color col;
  let p1,p2 = (Segment.extr1 s),(Segment.extr2 s) in
  moveto (iof (Point.x_coord p1)) (iof(Point.y_coord p1));
  lineto (iof(Point.x_coord p2))(iof (Point.y_coord p2))

 let draw_vector ?(lw=1) v d col =
   draw_segment ~lw:lw (Segment.make d (Vector.move_to v d)) col

let draw_triangle ?(lw=1) t col =
  let open Triangle in
  let (a,b,c) = Triangle.segments t in
  List.iter (fun e -> draw_segment ~lw:lw e col) [a;b;c]

let draw_rectangle ?(lw=1) r col =
  let open Rectangle in
  List.iter (fun e -> draw_segment ~lw:lw e col) (segments r)

let draw_ellipse ?(lw=1) e col =
  let open Point in
  let open Ellipse in
  set_line_width lw;
  set_color col;
  draw_ellipse (iof ((center e).x)) (iof ((center e).y)) (iof (big_axis e)) (iof (small_axis e))

let draw_string posx posy str col =
  set_color col;
  moveto posx posy;
  draw_string str

let draw_string_at_point pt str col =
  set_color col;
  moveto (Point.x_coord pt |> iof) (Point.y_coord pt |> iof);
  Graphics.draw_string str

let draw_regular ?(lw=1) rp col =
  let open Point in
  let open Polygon.Regular in
  set_color col;
  draw_point ~lw:3 rp.center col;
  set_line_width lw;
  moveto (iof rp.fst.x) (iof rp.fst.y);
  fold_stop (fun _ _ -> true)
    (fun nth _ current next ->
       draw_point ~lw:3 current col;
       set_line_width lw;
       lineto (iof next.x) (iof next.y)
    ) () rp

let draw_polygon ?(lw=1) (p: Polygon.t) col =
  let open Point in
  set_line_width lw;
  set_color col;
  moveto (iof (Polygon.first_point p).x) (iof (Polygon.first_point p).y);
  Polygon.fold (fun _ current next ->
      lineto (iof next.x) (iof next.y)) () p

let draw_line ?(lw=1) l col =
  let sx = float_of_int (size_x ())
  and sy = float_of_int (size_y ()) in
  let r = Rectangle.make Point.orig sx sy in
  let inter = Rectangle.intersect_line r l in
  match inter with
  | [a;b] -> draw_segment ~lw:lw (Segment.make a b) col
  | _ -> ()

let draw_polynom ?(lw=1) pol col =
  set_color col;
  set_line_width lw;
  let open Point in
  let open Polynom in
  let step = 5.
  and cur = ref 5. in
  moveto 0 (iof (equation pol 0.));
  while !cur < (float_of_int (size_x ())) do
    lineto (iof !cur) (iof (equation pol !cur));
    cur := !cur +. step
  done;
  lineto (iof !cur) (iof (equation pol !cur))

let draw_quadratic_curve ?(lw=1) curve col =
  set_color col;
  set_line_width lw;
  let open Point in
  let open Curve.Quadratic in
  moveto (iof (start curve).x) (iof (start curve).y);
  List.iter (fun e -> lineto (iof e.x) (iof e.y)) (points curve 50);
  lineto (iof (ending curve).x) (iof (ending curve).y)

let draw_cubic_curve ?(lw=1) curve col =
  set_color col;
  set_line_width lw;
  let open Point in
  let open Curve.Cubic in
  moveto (iof (start curve).x) (iof (start curve).y);
  List.iter (fun e -> lineto (iof e.x) (iof e.y)) (points curve 50);
  lineto  (iof (ending curve).x) (iof (ending curve).y)

let draw_bspline ?(lw=1) curve col =
  set_color col;
  set_line_width lw;
  let open Point in
  let open Curve.BSpline in
  let a  = (points curve 200) in
  moveto (iof (List.hd a).x) (iof (List.hd a).y);
  List.iter (fun e -> lineto (iof e.x) (iof e.y)) (List.tl a);
  let c = Circle.make (List.hd a) 5. in
  fill_circle c red
  (*lineto  (iof (ending curve).x) (iof (ending curve).y)*)

let open_graph size_x size_y title =
  let sx = size_x |> iof |> string_of_int
  and sy = size_y |> iof |> string_of_int in
  open_graph (" "^sx^"x"^sy);
  set_window_title title

let fill_screen rgb =
  set_color rgb;
  fill_rect 0 0 (size_x ()) (size_y ())

let clear () = fill_screen white

let get_image size_x size_y =
  let sx = size_x |> iof
  and sy = size_y |> iof in
  get_image 0 0 sx sy

let red i = i / 0x10000
let green i = (i / 0x100) mod 0x100
let blue i = i mod 0x100

let to_ppm img file_name =
  let open Printf in
  let arr = dump_image img in
  let row = Array.length arr |> soi
  and col = Array.length arr.(0) |> soi in
  let oc = open_out file_name in
  fprintf oc "%s\n" "P3";
  fprintf oc "%s %s\n" col row;
  fprintf oc "%s\n" "255";
  Array.iter
    (fun line ->
      Array.iter
	(fun e ->
	  fprintf oc "%i %i %i " (red e) (green e) (blue e)
	)
	line;
      fprintf oc "\n";
    )
    arr;
  close_out oc
