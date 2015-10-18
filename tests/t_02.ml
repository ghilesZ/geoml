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

let circumscribed t = 
  let (pa,pb,pc) = Triangle.points t in
  Circle.circumscribed pa pb pc

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
  Drawing.draw_circle (circumscribed !cur) Graphics.red

let handler status =
  let open Graphics in
  if status.key = 'r' then begin new_triangle (); frame () end
    
let loop state = 
  Graphics.loop_at_exit [Graphics.Key_pressed] handler

let doit la lb = 
  Drawing.open_graph la lb "Calculating the circumscribed circle of a triangle";
  frame ();
  loop ()

let _ = 
  doit 800. 700.
