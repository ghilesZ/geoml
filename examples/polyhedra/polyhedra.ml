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

let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax
  in Triangle.make p1 p2 p3

(****************** Main example **********************)
let size_x = 800.
and size_y = 600.
and title = "polyhedra simple tests"

let padding = 40.

type t = Polyhedron.t

let new_val () : t =
  let comp() =
    Constraint.(match Random.int 4 with
                | 0 -> Lt
                | 1 -> Gt
                | 2 -> Geq
                | _ -> Leq)
  in
  let cons() =
    let point() = gen_point padding (size_x-.padding) padding (size_y-.padding) in
    let line = Line.of_points (point()) (point()) in
    Drawing.draw_line line Graphics.blue;
    Constraint.(make line (comp()))
  in
  Polyhedron.make (list_make (fun _ -> cons()) 3)

let frame (p:t) =
  Drawing.fill_polyhedron p Graphics.red;
  Drawing.draw_string 25 585 "Press 'R' to refresh" Graphics.black

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
