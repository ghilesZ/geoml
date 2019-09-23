module Make
         (A:Arith.T)
         (P:Signatures.Point_Sig with type arith = A.t) = struct

  type arith = A.t
  type point = P.t

  type t = X of arith | Y of arith * arith (** linear equation type *)

  let print fmt = function
    | Y(a,b) -> Format.fprintf fmt "y=%ax +. %a" A.pp_print a A.pp_print b
    | X(c) -> Format.fprintf fmt "x=%a" A.pp_print c

type error = Parallel of t * t | Same_coordinates of point

exception Error of error

let print_error fmt e =
  let open Format in
  match e with
  | Parallel(l1,l2) -> fprintf fmt "Bad arguments : parallel lines %a and %a" print l1 print l2
  | Same_coordinates p -> fprintf fmt "Bad arguments : same coordinates %a" P.print p

open A

let make a b c =
  let nc = neg c in
  if b = zero then X(nc/.a)
  else Y((neg a)/.b,(nc/.b))

let make_x f = X(f)

let make_y a b = Y(a,b)

let x_axis = Y(zero,zero)

let y_axis = X(zero)

let is_vertical = function
  | X _ -> true
  | _ -> false

let is_horizontal = function
  | Y(x,_) -> x=zero
  | _ -> false

let get_coeff = function
  | Y(a,b) -> (a, minus_one, b)
  | X(c) -> (one, zero, neg c)

let to_string = function
  | X(a) -> ("x="^(A.to_string a))
  | Y(a,b) -> ("y="^(A.to_string a)^"x+"^(A.to_string b))

let of_points (p1:point) (p2:point) =
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
  | Y(a,b) when a <> zero-> (y-.b) /. a
  | _ -> raise (Error (Parallel(l,Y(zero,y))))

let y_from_x l x =
  match l with
  | X(_) -> raise (Error (Parallel(l,(make_x x))))
  | Y(a,b) -> a *. x +. b

let contains l (p:point) =
  let open P in
  match l with
  | X(n) -> p.x = n
  | Y(a,b) -> (a*.(p.x) +. b) = p.y

let scale_x l f =
  match l with
  | X(n) -> X(n*.f)
  | Y(a,b) -> Y(a*.f,b)

let scale_y l f =
  match l with
  | Y(a,b) -> Y (a*.f, b*.f)
  | _ -> l

let translate dx dy l =
  match l with
  | X(n) -> X(n+.dx)
  | Y(a,b) ->
    let p1 = P.translate dx dy (P.make zero b)
    and p2 = P.translate dx dy (P.make one (a+.b)) in
    of_points p1 p2

let parallel l1 l2 =
  match l1,l2 with
  | X(_), X(_) -> true
  | Y(a,_),Y(b,_) -> a = b
  | _ -> false

let intersects l1 l2 = parallel l1 l2 |> not

let intersection l1 l2 =
  match l1,l2 with
  | Y(a1,b1),Y(a2,b2) when a1<>a2->
     let x = (b2 -. b1) /. (a1 -. a2) in
     let y = a1 *. x +. b1 in
     P.make x y
  | Y(a,b), X(x) | X(x), Y(a,b) ->
     let y = a *. x +. b in P.make x y
  | _  -> raise (Error (Parallel(l1,l2)))


let perpendicular l1 l2 =
  match l1,l2 with
  | X(_),Y(x,_) | Y(x,_),X(_) -> x=zero
  | Y(a,_),Y(b,_) -> a *. b = minus_one
  | _ -> false

let perpendicular_of_line l p =
  let open P in
  match l with
  | Y(a,_)  ->
     if a = zero then X(p.x) else
     let coeff = minus_one /. a in
     let ord = p.y -. coeff *. p.x in
     Y(coeff,ord)
  | X(_) -> Y(zero,p.y)

let parallel_of_line l p =
  let open P in
  match l with
  | Y(a,_) ->
     let ord = p.y -. a *. p.x in
     Y(a,ord)
  | X(_) -> X(p.x)

let orth_proj l p =
  perpendicular_of_line l p |> intersection l

let point_bissection p1 p2 =
  P.center p1 p2 |> perpendicular_of_line (of_points p1 p2)

let arbitrary_point l =
  match l with
  | X(c) -> P.make c zero
  | Y(_,b) -> P.make zero b

end
