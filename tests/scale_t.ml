open Geom
open Utils

let size_x = 600. 
and size_y = 400.
let padding = 100.
and size = 10
and title = "Changing scale"
let cur = ref ((Rectangle.make (Point.make 10. 10.) 10. 10.),[])

let zoom_step = 0.5
let decale_step = 5.

let dx = ref 0.
let dy = ref 0.
let cur_zoom = ref 1.

let clear () = Drawing.fill_screen Graphics.white

let init () =
  let rect = (Rectangle.make (Point.make 22. 22.) 10. 10.)
  and xs = ref 5. and ys = ref 5. in 
  let x_lines = list_make (fun () -> let l = Line.make_x !xs in xs := !xs +. 5.; l) 120
  and y_lines = list_make (fun () -> let l = Line.make_y 0. !ys in ys := !ys +. 5.; l) 100 in
  rect,x_lines@y_lines
  
let frame (r,l) = 
  let r =  Rectangle.scale_y (Rectangle.scale_x r !cur_zoom) !cur_zoom
  and l = List.map (fun l -> Line.scale_y (Line.scale_x l !cur_zoom) !cur_zoom) l in
  let r = Rectangle.translate r !dx !dy
  and l = List.map (fun l -> Line.translate l !dx !dy) l in
  List.iter (fun e -> Drawing.draw_line e (Graphics.rgb 200 200 200)) l;
  Drawing.draw_rectangle r Graphics.blue

let zoom () = 
  cur_zoom := !cur_zoom +. zoom_step

let unzoom () =
  cur_zoom := !cur_zoom -. zoom_step

let translate d_x d_y = 
  dx := !dx +. (d_x *. !cur_zoom);
  dy := !dy +. (d_y *. !cur_zoom)

let handler status =
  let open Graphics in
  clear ();
  (match status.key with
  | '+' -> zoom ()
  | '-' -> unzoom()
  | 's' -> translate 0. decale_step
  | 'd' -> translate (-.decale_step) 0.
  | 'z' -> translate 0. (-.decale_step)
  | 'q' -> translate decale_step 0.
  | _ -> ());
  frame !cur

let loop state = 
  Graphics.loop_at_exit [Graphics.Key_pressed] handler
    
let doit () =
  Random.self_init ();
  Drawing.open_graph size_x size_y title;
  cur := init();
  frame !cur;
  loop ()

let _ = doit()


