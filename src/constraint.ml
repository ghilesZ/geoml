(**The linear constraint module*)
type comp = Lt | Gt | Leq | Geq

let neg = function
  | Lt  -> Geq
  | Leq -> Gt
  | Gt  -> Leq
  | Geq -> Lt

let compf = function
  | Lt  -> ( < )
  | Leq -> ( <= )
  | Gt  -> ( > )
  | Geq -> ( >= )

type t = Line.t * comp

let make l comp : t =
  (l,comp)

let get_border (l,_) = l

let get_comp (_,c) = c

let contains ((l,comp):t) p =
  let open Point in
  let (a,b,c) = Line.get_coeff l in
  let value = a *. p.x +. b *. p.y +. c in
  (compf comp) value 0.

(**contains c p returns true if the point p is in the half-space defined by c*)
let translate dx dy ((l,comp):t) =
  make (Line.translate dx dy l) comp

let intersects (((l1,comp1)as c1):t) (((l2,comp2) as c2) :t) =
  match comp1,comp2 with
  | x,y when Line.parallel l1 l2 ->
     let v1 = Line.arbitrary_point l1 in
     contains c2 v1 ||
       (let v2 = Line.arbitrary_point l2 in
	contains c1 v2)
  | _ -> true
