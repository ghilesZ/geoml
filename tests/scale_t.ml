open Geom
open Utils

let size_x = 600. 
and size_y = 400.
let padding = 100.
and size = 10
and title = "Scale and translation transformations"
let tr = Triangle.make (Point.make 50. 50.) (Point.make 50. 120.)  (Point.make 150. 110.)
let rect = Rectangle.make (Point.make 100. 100.) 100. 100.
let ell = Ellipse.make_bifocal (Point.make 120. 120.) (Point.make 220. 120.) 20.
let cur = ref (rect,tr,ell,[])

let zoom_step = 0.5
let decale_step = 5.

let dx = ref 0.
let dy = ref 0.
let cur_zoom = ref 1.

let clear () = Drawing.fill_screen Graphics.white

let init () =
  let xs = ref (-500.) and ys = ref (-500.) in 
  let x_lines = list_make (fun () -> let l = Line.make_x !xs in xs := !xs +. 5.; l) 1200
  and y_lines = list_make (fun () -> let l = Line.make_y 0. !ys in ys := !ys +. 5.; l) 1000 in
  rect,tr,ell,x_lines@y_lines
  
let frame (r,tr,ell,l) = 
  let r =  Rectangle.scale_y (Rectangle.scale_x r !cur_zoom) !cur_zoom
  and tr = Triangle.scale_y (Triangle.scale_x tr !cur_zoom) !cur_zoom
  and ell = Ellipse.scale_y (Ellipse.scale_x ell !cur_zoom) !cur_zoom
  and l = List.map (fun l -> Line.scale_y (Line.scale_x l !cur_zoom) !cur_zoom) l in
  let r = Rectangle.translate !dx !dy r
  and tr = Triangle.translate !dx !dy tr
  and ell = Ellipse.translate !dx !dy ell
  and l = List.map (fun l -> Line.translate !dx !dy l) l in
  List.iter (fun e -> Drawing.draw_line e (Graphics.rgb 230 230 230)) l;
  Drawing.draw_string 20 380 "'+'/'-' to zoom/unzoom" Graphics.black;
  Drawing.draw_string 20 360 "'z' 'q' 's' 'd' to move " Graphics.black;
  Drawing.draw_triangle tr Graphics.red;
  Drawing.draw_ellipse ell Graphics.green;
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


