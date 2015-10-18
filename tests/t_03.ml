let size_x = ref 800. and size_y = ref 700. and size = ref 50

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
  let l = Array.make !size 0 |> Array.to_list|>
      List.map (fun _ ->
	gen_point 200. (!size_x-.200.) 200. (!size_y-.200.)
      ) 
  in ref l

let new_list () = 
  cur := List.map (fun _ ->
    gen_point 200. (!size_x-.200.) 200. (!size_y-.200.)
  ) (!cur)

(************************************************)

let bounding t = Circle.bounding t

(***************************************************)

let clear () = 
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 (iof !size_x) (iof !size_y)

let frame () =
  clear ();
  Drawing.draw_string 25 675 "Press 'r' to generate a new cloud" Graphics.black;
  List.iter (fun e -> 
    let c = Circle.make e 5. in 
    Drawing.fill_circle c Graphics.blue
  ) !cur;
  Drawing.draw_circle (bounding !cur) Graphics.red

let handler status =
  let open Graphics in
  if status.key = 'r' then begin new_list (); frame () end
    
let loop state = 
  Graphics.loop_at_exit [Graphics.Key_pressed] handler

let doit la lb nb = 
  size:=nb;
  Drawing.open_graph la lb "Calculating the bounding circle of a point list";
  frame ();
  loop ()

let _ = 
  doit 800. 700. 50
