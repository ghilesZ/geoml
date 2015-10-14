open Graphics

let iof = int_of_float

let draw_point ?(lw=1) p col =
  set_color col;
  set_line_width lw;
  fill_circle (Point.x_coord p |> iof) (Point.y_coord p |> iof) lw

let draw_circle ?(lw=1) c col =
  set_color col;
  set_line_width lw;
  draw_circle 
    (c |> Circle.center |> Point.x_coord |> iof)
    (c |> Circle.center |> Point.y_coord |> iof)
    (c |> Circle.radius |> iof)

let fill_circle ?(lw=1) c col =
  set_color col;
  set_line_width lw;
  fill_circle 
    (c |> Circle.center |> Point.x_coord |> iof)
    (c |> Circle.center |> Point.y_coord |> iof)
    (c |> Circle.radius |> iof)
  
let draw_segment ?(lw=1) s col =
  set_line_width lw;
  let p1,p2 = (Segment.extr1 s),(Segment.extr2 s) in
  set_color col;
  moveto (iof (Point.x_coord p1)) (iof(Point.y_coord p1));
  lineto (iof(Point.x_coord p2))(iof (Point.y_coord p2))

let draw_triangle ?(lw=1) t col = 
  let open Triangle in
  tri_iter (fun e -> draw_segment ~lw:lw e col) (segments t);
  tri_iter (fun e -> draw_point ~lw:lw e col) (points t)

let draw_string posx posy str col =
  set_color col;
  Graphics.moveto posx posy;
  Graphics.draw_string str
  
let open_graph size_x size_y title =
  let sx = size_x |> iof |> string_of_int
  and sy = size_y |> iof |> string_of_int in
  open_graph (" "^sx^"x"^sy);
  set_window_title title
