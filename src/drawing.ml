open Geoml
open Graphics

(* conversion utilities *)
let iof = int_of_float
let soi = string_of_int

let point2pixel p = Point.(iof p.x, iof p.y)

let moveto_p p =
  let x,y = point2pixel p in
  moveto x y

let lineto_p p =
  let x,y = point2pixel p in
  lineto x y

(* screen handling *)
let open_graph size_x size_y title =
  let sx = size_x |> iof |> string_of_int
  and sy = size_y |> iof |> string_of_int in
  open_graph (" "^sx^"x"^sy);
  set_window_title title

let fill_screen rgb =
  set_color rgb;
  fill_rect 0 0 (size_x ()) (size_y ())

let clear = clear_graph

let screen () =
 let sx = float_of_int (size_x ())
 and sy = float_of_int (size_y ()) in
 Rectangle.make Point.orig sx sy

let get_image size_x size_y =
  let sx = size_x |> iof
  and sy = size_y |> iof in
  get_image 0 0 sx sy

let red_c i = i / 0x10000
let green_c i = (i / 0x100) mod 0x100
let blue_c i = i mod 0x100

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
	  fprintf oc "%i %i %i " (red_c e) (green_c e) (blue_c e)
	)
	line;
      fprintf oc "\n";
    )
    arr;
  close_out oc

(* drawing *)
let draw_point ?(lw=1) p col =
  set_color col;
  set_line_width lw;
  let x,y = point2pixel p in
  fill_circle x y lw

let draw_segment ?(lw=1) s col =
  set_line_width lw;
  set_color col;
  let p1,p2 = (Segment.extr1 s),(Segment.extr2 s) in
  moveto_p p1;
  lineto_p p2

let draw_vector ?(lw=1) v d col =
   draw_segment ~lw:lw (Segment.make d (Vector.move_to v d)) col

let draw_dashed_segment ?(lw=1) s col =
  let step = 5. in
  let nb = Segment.size s /. step in
  let v = Vector.of_points (Segment.extr1 s) (Segment.extr2 s) in
  let v = Vector.scal_mult (1./.nb) v in
  let rec loop p nb =
    if nb > 0. then begin
        draw_vector ~lw:lw v p col;
        loop (Vector.move_to (Vector.add v v) p) (nb -. 2.)
      end
  in
  loop (Segment.extr1 s) nb

let draw_line ?(lw=1) ?(dashed=false) l col =
  let sx = float_of_int (size_x ())
  and sy = float_of_int (size_y ()) in
  let r = Rectangle.make Point.orig sx sy in
  let inter = Rectangle.intersect_line r l in
  match inter with
  | [a;b] -> (if dashed then draw_dashed_segment else draw_segment)
               ~lw:lw (Segment.make a b) col
  | _ -> ()

let draw_constraint ?(lw=1) c col =
  let open Constraint in
  let l = get_border c
  and dashed = match (get_comp c) with
    | Lt | Gt -> true
    | _ ->  false
  in draw_line ~lw:lw ~dashed:dashed l col

(* shapes with non null area *)
let draw_circle ?(lw=1) c col =
  let open Circle in
  set_color col;
  set_line_width lw;
  let x,y = point2pixel c.center in
  draw_circle x y (c.radius |> iof)

let fill_circle ?(lw=1) c col =
  let open Circle in
  set_color col;
  set_line_width lw;
  let x,y = point2pixel c.center in
  fill_circle x y (c.radius |> iof)

let draw_triangle ?(lw=1) t col =
  let (a,b,c) = Triangle.segments t in
  List.iter (fun e -> draw_segment ~lw:lw e col) [a;b;c]

let draw_rectangle ?(lw=1) r col =
  let open Rectangle in
  List.iter (fun e -> draw_segment ~lw:lw e col) (segments r)

let draw_ellipse ?(lw=1) e col =
  let open Ellipse in
  set_line_width lw;
  set_color col;
  let x,y = point2pixel (center e) in
  draw_ellipse x y (iof (big_axis e)) (iof (small_axis e))

let draw_regular ?(lw=1) rp col =
  let open Polygon.Convex.Regular in
  set_color col;
  draw_point ~lw:3 rp.center col;
  set_line_width lw;
  moveto_p rp.fst;
  fold_stop (fun _ _ -> true)
    (fun _ _ current next ->
       draw_point ~lw:3 current col;
       set_line_width lw;
       lineto_p next
    ) () rp

let draw_polygon ?(lw=1) (p: Polygon.t) col =
  set_line_width lw;
  set_color col;
  moveto_p Polygon.(first_point p);
  Polygon.fold (fun _ _ -> lineto_p) () p

let fill_polygon ?(lw=1) (p: Polygon.t) col =
  set_line_width lw;
  set_color col;
  let pts_array = List.map point2pixel (Polygon.to_list p) |> Array.of_list in
  fill_poly pts_array

let draw_convex_polygon ?(lw=1) (p: Polygon.Convex.t) col =
  set_line_width lw;
  set_color col;
  let pts = Polygon.Convex.to_list p in
  moveto_p (List.hd pts);
  List.iter (fun current -> lineto_p current) (List.tl pts)

let fill_convex_polygon ?(lw=1) (p: Polygon.Convex.t) col =
  set_line_width lw;
  set_color col;
  let pts_array = List.map point2pixel (Polygon.Convex.to_list p)
                  |> Array.of_list in
  fill_poly pts_array

let draw_polyhedron ?(lw=1) polyhedron col =
  let open Polyhedron in
  List.iter (fun c -> draw_constraint ~lw:lw c col) (get_constr polyhedron)

let fill_polyhedron ?(lw=1) plhd col =
  let open Polyhedron in
  let r = screen() in
  let s = Rectangle.([bottom_left_corner r;
                      bottom_right_corner r;
                      top_left_corner r;
                      top_right_corner r;
          ]) in
  let screen_pol = Polygon.Convex.hull s in
  let screen_phd = of_polygon screen_pol in
  let plhd = intersection plhd screen_phd in
  if not (Polyhedron.is_empty plhd) then begin
      let plg = to_polygon plhd in
      set_line_width lw;
      set_color col;
      let pts_array = List.map point2pixel (Polygon.Convex.to_list plg) |> Array.of_list in
      fill_poly pts_array
    end
  else Format.printf "empty polyhedron : %a\n%!" Polyhedron.print plhd

let draw_polynom ?(lw=1) pol col =
  set_color col;
  set_line_width lw;
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
  let open Curve.Quadratic in
  moveto_p (start curve);
  List.iter lineto_p (points curve 50);
  lineto_p (ending curve)

let draw_cubic_curve ?(lw=1) curve col =
  set_color col;
  set_line_width lw;
  let open Curve.Cubic in
  moveto_p (start curve);
  List.iter lineto_p (points curve 50);
  lineto_p (ending curve)

let draw_bspline ?(lw=1) curve col =
  set_color col;
  set_line_width lw;
  let open Curve.BSpline in
  let a = (points curve 200) in
  moveto_p (List.hd a);
  List.iter lineto_p (List.tl a);
  let c = Circle.make (List.hd a) 5. in
  fill_circle c red

(* string drawing *)
let draw_string posx posy str col =
  set_color col;
  moveto posx posy;
  draw_string str

let draw_string_at_point pt str col =
  set_color col;
  moveto_p pt;
  Graphics.draw_string str
