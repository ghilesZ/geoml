(**The linear constraint module*)
let arbitrary_point l =
  let open Line in
  match l with
  | X(c) -> Point.make c 0.
  | Y(a,b) -> Point.make 0. b
     
type comp = Lt | Gt | Leq | Geq

let neg = function
  | Lt -> Geq
  | Leq -> Gt
  | Gt -> Leq
  | Geq -> Lt
    
type t = Line.t * comp

let make l comp : t =
  (l,comp)

let get_border l = fst l
    
let contains ((l,comp):t) p =
  let open Point in
  let (a,b,c) = Line.get_coeff l in
  let value = a *. p.x +. b *. p.y +. c in
  match comp with
  | Lt -> value < 0.
  | Leq ->value  <= 0.
  | Gt -> value > 0.
  | Geq -> value >= 0.
(**contains c p returns true if the point p is in the half-space defined by c*)

let translate ((l,comp):t) dx dy =
  make (Line.translate l dx dy) comp

let intersects (((l1,comp1)as c1):t) (((l2,comp2) as c2) :t) = 
  match comp1,comp2 with
  | x,y when Line.parallel l1 l2 ->
     let v1 = arbitrary_point l1 in
     contains c2 v1 ||
       (let v2 = arbitrary_point l2 in
	contains c1 v2)
  | _ -> true
