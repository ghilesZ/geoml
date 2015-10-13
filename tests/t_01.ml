let size_x = ref 800. and size_y = ref 700.
let col_point = Graphics.rgb 120 120 230
let col_fermat = Graphics.rgb 230 120 120
let col_line = Graphics.rgb 90 190 220

let iof = int_of_float

let () = Random.self_init ()

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

let cur = ref (gen_triangle 200. (!size_x-.200.) 200. (!size_y-.200.))

let new_triangle () = 
  cur := (gen_triangle 200. (!size_x-.200.) 200. (!size_y-.200.))

(************************************************)

let fermat t =
  let build_pts seg = 
    let piv = Segment.extr1 seg 
    and pt = Segment.extr2 seg in
    (Point.rotate piv pt 1.0472),(Point.rotate piv pt (-.1.0472))
  in
  let further p1 (p2,p3) =
    if Point.sq_distance p1 p2 > Point.sq_distance p1 p3 then p2
    else p3
  in
  let (a,b,c) = Triangle.points t in
  let bc = Segment.make b c
  and ac = Segment.make a c in
  let l1 = Line.of_points (build_pts bc |> further a) a
  and l2 = Line.of_points (build_pts ac |> further b) b
  in Line.intersection l1 l2

let fermat t = 
  let (pa,pb,pc) = Triangle.points t in
  let (a,b,c) = Triangle.angles t in
  if a > 120. then pa
  else if b > 120. then pb
  else if c > 120. then pc
  else fermat t


(************************************************)

let draw_point p col_point =
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
  tri_iter (fun e -> draw_point e col_point) (points t)

let draw_cur () = draw_triangle !cur

let clear () = 
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 (iof !size_x) (iof !size_y)

let frame () =
  clear ();
   draw_triangle !cur;
   Graphics.rgb 250 120 120 |>(fermat !cur |> draw_point)

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
  doit 800 700
