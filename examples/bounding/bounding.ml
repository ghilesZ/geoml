open Geoml

let iof = int_of_float
let foi = float_of_int

let list_make f s =
  let rec aux res = function
  | 0 -> res
  | n -> aux ((f ())::res) (n-1)
  in aux [] s

(*************** Random utilities *******************)
let gen_point xmin xmax ymin ymax =
  let x = xmin +. (foi (Random.int (iof (xmax-.xmin))))
  and y = ymin +. (foi (Random.int (iof (ymax-.ymin)))) in
  Point.make x y



let size_x = 1800.
and size_y = 1000.
and size = 50
and padding = 100.
and title = "Calculating the bounding circle/rectangle/polygon of a point list"

(*************** Random utilities *******************)
let new_val () = list_make
                   (fun _ -> gen_point padding (size_x-.padding)
                               padding (size_y-.padding))
                   size

let frame v =
  Drawing.draw_string 25 25 "Press 'r' to generate a new cloud" Graphics.black;
  Drawing.draw_convex_polygon (Polygon.Convex.hull v) Graphics.blue;
  Drawing.draw_circle (Circle.bounding v) Graphics.red;
  Drawing.draw_rectangle (Rectangle.bounding v) Graphics.green;
  List.iter (fun e ->
      let c = Circle.make e 3. in
      Drawing.fill_circle c Graphics.black
    ) v

(****** Window and event handling *******************)
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
