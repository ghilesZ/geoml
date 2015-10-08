type t = X of float | Y of float * float

let make_x f = X(f)

let make_y a b = Y(a,b)

let to_string = function
  | X(a) -> ("x="^(string_of_float a))
  | Y(a,b) -> ("y="^(string_of_float a)^"x+"^(string_of_float b))

let of_points p1 p2 =
  if p1 = p2 then 
    failwith "Line.of_points: points have same coordinate, can't build line whith those points" 
  else if Point.x_coord p1 = Point.x_coord p2 then
    X(Point.x_coord p1)
  else 
    let x1,x2 = (Point.x_coord p1), (Point.x_coord p2)
    and y1,y2 = (Point.y_coord p1), (Point.y_coord p2) in
    let coeff = (y2 -. y1) /. (x2 -. x1) in
    let ord = y1 -. coeff *. x1
    in Y(coeff,ord)

let parallel l1 l2 = 
  match l1,l2 with
  | X(_), X(_) -> true
  | Y(a,_),Y(b,_) when a = b -> true
  | _ -> false
