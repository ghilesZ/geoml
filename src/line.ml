type t = X of float | Y of float * float

let make_x f = X(f)

let make_y a b = Y(a,b)

let to_string = function
  | X(a) -> ("x="^(string_of_float a))
  | Y(a,b) -> ("y="^(string_of_float a)^"x+"^(string_of_float b))

let of_points (p1:Point.t) (p2:Point.t) =
  let open Point in
  if p1 = p2 then 
    failwith "Line.of_points: points have same coordinate, can't build line whith those points" 
  else if p1//x = p2//x then
    X(p1//y)
  else 
    let coeff = (p2//y -. p1//y) /. (p2//x -. p1//x) in
    let ord = p1//y -. coeff *. (p1//x)
    in Y(coeff,ord)

let parallel l1 l2 = 
  match l1,l2 with
  | X(_), X(_) -> true
  | Y(a,_),Y(b,_) when a = b -> true
  | _ -> false

let perpendicular l1 l2 = 
  match l1,l2 with
  | X(_),Y(0.,_) | Y(0.,_),X(_) -> true
  | Y(a,_),Y(b,_) when a *. b = -1. -> true
  | _ -> false

let contains (l:t) (p:Point.t) = 
  let open Point in
  match l with
  | X(n) -> p//x = n
  | Y(a,b) -> (a*.(p//x) +. b) = p//y
 
