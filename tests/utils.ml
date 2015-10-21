let iof = int_of_float
let foi = float_of_int

let gen_point xmin xmax ymin ymax =
  let x = xmin +. Random.float (xmax-.xmin)
  and y = ymin +. Random.float (ymax-.ymin) in
  Point.make x y

let list_make f s =
  let rec aux res = function
  | 0 -> res
  | n -> aux ((f ())::res) (n-1)
  in aux [] s

let gen_point xmin xmax ymin ymax =
  let x = xmin +. (foi (Random.int (int_of_float (xmax-.xmin))))
  and y = ymin +. (foi (Random.int (int_of_float (ymax-.ymin)))) in
  Point.make x y
    
let gen_triangle xmin xmax ymin ymax =
  let p1 = gen_point xmin xmax ymin ymax
  and p2 = gen_point xmin xmax ymin ymax
  and p3 = gen_point xmin xmax ymin ymax
  in Triangle.make p1 p2 p3
