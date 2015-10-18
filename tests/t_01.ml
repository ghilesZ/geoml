let size_x = ref 800. and size_y = ref 700.

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

let cur = 
  Random.self_init ();
  ref (gen_triangle 200. (!size_x-.200.) 200. (!size_y-.200.))

let new_triangle () = 
  cur := (gen_triangle 200. (!size_x-.200.) 200. (!size_y-.200.))

(************************************************)

let aux t =
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
  and ac = Segment.make a c 
  and ab = Segment.make a b in
  
  let far_a = build_pts bc |> further a
  and far_b = build_pts ac |> further b
  and far_c = build_pts ab |> further c in

  Drawing.fill_circle (Circle.make far_a 4.) (Graphics.rgb 200 230 200);
  Drawing.fill_circle (Circle.make far_b 4.) (Graphics.rgb 200 230 200);
  Drawing.fill_circle (Circle.make far_c 4.) (Graphics.rgb 200 230 200);
 
  Drawing.draw_segment (Segment.make b far_a) (Graphics.rgb 210 240 210);
  Drawing.draw_segment (Segment.make c far_a) (Graphics.rgb 210 240 210);

  Drawing.draw_segment (Segment.make a far_b) (Graphics.rgb 210 240 210);
  Drawing.draw_segment (Segment.make c far_b) (Graphics.rgb 210 240 210);

  Drawing.draw_segment (Segment.make a far_c) (Graphics.rgb 210 240 210); 
  Drawing.draw_segment (Segment.make b far_c) (Graphics.rgb 210 240 210);

  let l1 = Line.of_points far_a a
  and l2 = Line.of_points far_b b
  and l3 = Line.of_points far_c c in
 
  let l1_l2 = Line.intersection l1 l2
  and l2_l3 = Line.intersection l2 l3
  and l3_l1 = Line.intersection l3 l1 in

  Drawing.draw_line  l1 (Graphics.rgb 230 180 230);
  Drawing.draw_line  l2 (Graphics.rgb 230 180 230);
  Drawing.draw_line  l3 (Graphics.rgb 230 180 230);
 
  Point.barycenter [l1_l2; l2_l3; l3_l1]

let fermat t = 
  let (pa,pb,pc) = Triangle.points t in
  let (a,b,c) = Triangle.angles t in
  if a > 120. then pa
  else if b > 120. then pb
  else if c > 120. then pc
  else aux t

(***************************************************)

let clear () = 
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 (iof !size_x) (iof !size_y)

let frame () =
  clear ();
  Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;
  Drawing.draw_triangle ~lw:2 !cur Graphics.blue;
  Triangle.tri_iter (fun e -> 
    let c = Circle.make e 5. in
    Drawing.fill_circle c Graphics.green
  ) (Triangle.points !cur );
  Drawing.fill_circle (Circle.make (fermat !cur) 6.) Graphics.red

let handler status =
  let open Graphics in
  if status.key = 'r' then begin new_triangle (); frame () end
    
let loop state = 
  Graphics.loop_at_exit [Graphics.Key_pressed] handler

let doit la lb = 
  Drawing.open_graph la lb "Calculating the fermat point of a triangle";
  frame ();
  loop ()

let _ = 
  doit 800. 700.
