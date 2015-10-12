let size_x = 800. and size_y = 700.
let col_point = Graphics.rgb 120 120 230
let col_fermat = Graphics.rgb 230 120 120
let col_line = Graphics.rgb 190 190 220

let gen_point xmin xmax ymin ymax =
  let x = xmin +. Random.float (xmax-.xmin)
  and y = xmax +. Random.float (ymax-.ymin) in
  Point.make x y

let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax
  in Triangle.make p1 p2 p3

let cur = ref (gen_triangle 0. size_x 0. size_y)

let new_triangle () = 
  cur := (gen_triangle 0. size_x 0. size_y)

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
  let x = x_coord p |> int_of_float
  and y = y_coord p |> int_of_float
  in Graphics.fill_circle x y 2

let draw_segment e = 
  let p1,p2 = (Segment.extr1 e),(Segment.extr2 e) in 
  ()

let draw_triangle t = 
  let open Triangle in
  tri_iter draw_segment (segments t)

let draw_cur () = ()

let clear () = ()

let frame () =
  clear ();
  draw_triangle !cur

let handler status = ()
    
let loop state = 
  Graphics.loop_at_exit [Key_pressed] handler

let doit la lb = let open Graphics in
  open_graph (" "^(string_of_int la)^"x"^(string_of_int lb));
  set_window_title "Calculating the fermat point of a triangle";
  frame ();
  loop ()

let _ = 
  Random.self_init();
  doit 800 700
