let size_x = ref 800. and size_y = ref 700.

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

(***************************************************)

let clear () = 
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 (iof !size_x) (iof !size_y)

let frame () =
  clear ();
  Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;
  Drawing.draw_triangle !cur Graphics.blue;
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
