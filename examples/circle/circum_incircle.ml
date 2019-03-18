open Geoml

(*************** Drawing utilities ******************)



(*************** Random utilities *******************)
let iof = int_of_float
let foi = float_of_int

let gen_point xmin xmax ymin ymax =
  let x = xmin +. (foi (Random.int (int_of_float (xmax-.xmin))))
  and y = ymin +. (foi (Random.int (int_of_float (ymax-.ymin)))) in
  Point.make x y

let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax
  in Triangle.make p1 p2 p3


(****************************************************)

let size_x = 800.
and size_y = 700.
and title = "Calculating the circumscribed circle and the incircle of a triangle"

let new_val () = gen_triangle 200. (size_x-.200.) 200. (size_y-.200.)

let work t =
  let (pa,pb,pc) = Triangle.points t in
  let inc = (Circle.incircle pa pb pc) in
  (Circle.circumscribed pa pb pc), inc

let frame v =
  Drawing.draw_string 25 675 "Press 'r' to generate a new triangle" Graphics.black;
  Drawing.draw_triangle ~lw:1 v Graphics.blue;
  Triangle.tri_iter (fun e ->
      let c = Circle.make e 5. in
      Drawing.fill_circle c Graphics.green
    ) v;
  let (c1,c2) = (work v) in
  Drawing.draw_circle c1 Graphics.red;
  Drawing.draw_circle c2 Graphics.blue

(****** Window and event handling *****)
let clear () = Drawing.fill_screen Graphics.white

let handler status =
  let open Graphics in
  if status.key = 'r' then begin
      clear ();
      new_val () |> frame
    end

let loop () =
  Graphics.loop_at_exit [Graphics.Key_pressed] handler

let doit () =
  Random.self_init ();
  Drawing.open_graph size_x size_y title;
  new_val () |> frame;
  loop ()

let () = doit()
