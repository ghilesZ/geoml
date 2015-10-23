type t = X of float | Y of float * float (** linear equation type *)

exception Vertical of float
exception Parallel

let make_x f = X(f)

let make_y a b = Y(a,b)

let is_vertical = function 
  | X(_) -> true
  | _ -> false

let get_coeff = function
  | Y(a,_) -> a
  | X(v) -> raise (Vertical(v))

let get_ord = function
  | Y(_,a) -> a
  | X(v) -> raise (Vertical(v))

let to_string = function
  | X(a) -> ("x="^(string_of_float a))
  | Y(a,b) -> ("y="^(string_of_float a)^"x+"^(string_of_float b))

let of_points (p1:Point.t) (p2:Point.t) =
  let open Point in
  if p1 = p2 then 
    failwith "Line.of_points: points have same coordinate, can't build line whith those points" 
  else if p1.x = p2.x then
    X(p1.y)
  else 
    let coeff = (p2.y -. p1.y) /. (p2.x -. p1.x) in
    let ord = p1.y -. coeff *. (p1.x)
    in Y(coeff,ord)

let x_from_y l y =
  match l with
  | X(x) -> x
  | Y(a,b) -> (y-.b) /. a

let y_from_x l x =
  match l with
  | X(c) -> raise (Vertical(c))
  | Y(a,b) -> a *. x +. b

let contains (l:t) (p:Point.t) = 
  let open Point in
  match l with
  | X(n) -> p.x = n
  | Y(a,b) -> (a*.(p.x) +. b) = p.y

let translate (l:t) dx dy = 
  match l with 
  | X(n) -> X(n+.dx)
  | Y(a,b) -> 
    let p1 = Point.translate (Point.make 0. b) dx dy
    and p2 = Point.translate (Point.make 1. (a+.b)) dx dy in
    of_points p1 p2

let parallel l1 l2 = 
  match l1,l2 with
  | X(_), X(_) -> true
  | Y(a,_),Y(b,_) when a = b -> true
  | _ -> false

let intersects l1 l2 = parallel l1 l2 |> not

let intersection l1 l2 = 
  match l1,l2 with 
  | Y(a1,b1),Y(a2,b2) when a1<>a2-> 
     let x = (b2 -. b1) /. (a1 -. a2) in
     let y = a1 *. x +. b1 in
     Point.make x y
  | Y(a,b), X(x) | X(x), Y(a,b) -> 
     let y = a *. x +. b in Point.make x y
  | _  -> raise Parallel

let perpendicular l1 l2 = 
  match l1,l2 with
  | X(_),Y(0.,_) | Y(0.,_),X(_) -> true
  | Y(a,_),Y(b,_) when a *. b = -1. -> true
  | _ -> false

let perpendicular_of_line l p = 
  let open Point in
  match l with 
  | Y(0.,b) -> X(p.x)
  | Y(a,b)  ->
     let coeff = (-.1.) /. a in
     let ord = p.y -. coeff *. p.x in
     Y(coeff,ord)
  | X(_) -> Y(0.,p.y)

let parallel_of_line l p = 
  let open Point in
  match l with 
  | Y(a,b) ->
     let ord = p.y -. a *. p.x in
     Y(a,ord)
  | X(x) -> X(p.x)

let orth_proj l p =
  perpendicular_of_line l p |> intersection l

let point_bissection p1 p2 = 
  Point.center p1 p2 |> perpendicular_of_line (of_points p1 p2)
