type t = X of float | Y of float * float (** linear equation type *)

let print fmt = function
  | Y(a,b) -> Format.fprintf fmt "y=%fx +. %fb" a b
  | X(c) -> Format.fprintf fmt "x=%f" c

type error = Parallel of t * t | Same_coordinates of Point.t
exception Error of error
let print_error fmt e =
  let open Format in
  match e with
  | Parallel(l1,l2) -> fprintf fmt "Bad arguments : parallel lines %a and %a" print l1 print l2
  | Same_coordinates p -> fprintf fmt "Bad arguments : same coordinates %a" Point.print p

let make a b c =
  if b = 0. then X(-.c/.a)
  else Y((-.a/.b),(-.c/.b))

let make_x f = X(f)

let make_y a b = Y(a,b)

let x_axis = Y(0.,0.)

let y_axis = X(0.)
  
let is_vertical = function 
  | X(_) -> true
  | _ -> false

let is_horizontal = function 
  | Y(0.,_) -> true
  | _ -> false

let get_coeff = function
  | Y(a,b) -> (a, -1., b)
  | X(c) -> (1., 0., -.c)

let to_string = function
  | X(a) -> ("x="^(string_of_float a))
  | Y(a,b) -> ("y="^(string_of_float a)^"x+"^(string_of_float b))

let of_points (p1:Point.t) (p2:Point.t) =
  let open Point in
  if p1 = p2 then 
    raise (Error (Same_coordinates p1))
  else if p1.x = p2.x then
    X(p1.x)
  else 
    let coeff = (p2.y -. p1.y) /. (p2.x -. p1.x) in
    let ord = p1.y -. coeff *. (p1.x)
    in Y(coeff,ord)

let x_from_y l y =
  match l with
  | X(x) -> x
  | Y(a,b) when a <> 0.-> (y-.b) /. a
  | _ -> raise (Error (Parallel(l,Y(0.,y))))
     
let y_from_x l x =
  match l with
  | X(c) -> raise (Error (Parallel(l,(make_x x))))
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
  | _  -> raise (Error (Parallel(l1,l2)))


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
