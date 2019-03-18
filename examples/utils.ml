open Geom

let iof = int_of_float
let foi = float_of_int

let list_make f s =
  let rec aux res = function
  | 0 -> res
  | n -> aux ((f ())::res) (n-1)
  in aux [] s

let gen_point xmin xmax ymin ymax =
  let x = xmin +. (foi (Random.int (iof (xmax-.xmin))))
  and y = ymin +. (foi (Random.int (iof (ymax-.ymin)))) in
  Point.make x y

let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax
  in Triangle.make p1 p2 p3

let rec gen_line xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax in
  try Line.of_points p1 p2
  with Line.Error (Line.Same_coordinates ( _)) -> gen_line xmin xmax ymin ymax

let gen_polygon xmin xmax ymin ymax =
  let edges = 3 + Random.int 17 in
  let fst = gen_point xmin xmax ymin ymax in
  let center = gen_point xmin xmax ymin ymax in
  let rp = Polygon.Convex.Regular.make center fst edges in
  Polygon.Convex.Regular.to_randomized_polygon ~prob:0.7 rp

let gen_regular xmin xmax ymin ymax =
  let edges = 3 + Random.int 17 in
  let fst = gen_point xmin xmax ymin ymax in
  let center = gen_point xmin xmax ymin ymax in
  Polygon.Convex.Regular.make center fst edges
