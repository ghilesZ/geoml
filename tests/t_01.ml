let size_x = ref 800. and size_y = ref 700.
let col_point = Graphics.rgb 120 120 230
let col_fermat = Graphics.rgb 230 120 120
let col_line = Graphics.rgb 90 190 220

let iof = int_of_float

(************* RANDOM GENERATION ****************)

let gen_point xmin xmax ymin ymax =
  let x = xmin +. Random.float (xmax-.xmin)
  and y = ymin +. Random.float (ymax-.ymin) in
  Point.make x y

let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax
  in Triangle.make p1 p2 p3

let cur = ref (gen_triangle 0. !size_x 0. !size_y)

let new_triangle () = 
  cur := (gen_triangle 0. !size_x 0. !size_y)

(************************************************)

let exists_gt_120 t = false

let fermat t =
  if exists_gt_120 t then
    ()
  else
    ()

(************************************************)

let draw_point p =
  Graphics.set_color col_point;
  let open Point in
  let x = x_coord p |> iof
  and y = y_coord p |> iof
  in Graphics.fill_circle x y 10

let draw_segment e = 
  let p1,p2 = (Segment.extr1 e),(Segment.extr2 e) in
  Graphics.set_color col_line;
  Graphics.moveto (iof (Point.x_coord p1)) (iof(Point.y_coord p1));
  Graphics.lineto (iof(Point.x_coord p2))(iof (Point.y_coord p2))

let draw_triangle t = 
  let open Triangle in
  tri_iter draw_segment (segments t);
  tri_iter draw_point (points t)

let draw_cur () = draw_triangle !cur

let clear () = 
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 (iof !size_x) (iof !size_y)

let frame () =
  clear ();
  draw_triangle !cur

let handler status =
  let open Graphics in
  if status.key = 'r' then begin new_triangle (); frame () end
    
let loop state = 
  Graphics.loop_at_exit [Graphics.Key_pressed] handler

let doit la lb = 
  size_x := float_of_int la;
  size_y := float_of_int lb;
  let open Graphics in
  open_graph (" 800x700");
  set_window_title "Calculating the fermat point of a triangle";
  frame ();
  loop ()

let _ = 
  Random.self_init();
  doit 800 700
