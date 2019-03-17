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

let print fmt ((l,cmp):t) =
  let comp_to_string (c:comp) =
    match c with
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
  in
  let (a,b,c) = Line.get_coeff l in
  Format.fprintf fmt "%fx + %fy + %f %s 0" a b c (comp_to_string cmp)

let make l comp : t = (l,comp)

let get_border (l,_) = l

let get_comp (_,c) = c

let contains ((l,comp):t) p =
  let (a,b,c) = Line.get_coeff l in
  let value =
    let open Point in
    a *. p.x +. b *. p.y +. c
  in
  (compf comp) value 0.

(**contains c p returns true if the point p is in the half-space defined by c*)
let translate dx dy ((l,comp):t) =
  make (Line.translate dx dy l) comp

let complementary (l,comp) = l,(neg comp)

let intersects (((l1,_)as c1):t) (((l2,_) as c2) :t) =
  not(Line.parallel l1 l2)
  || Line.arbitrary_point l1 |> contains c2
  || Line.arbitrary_point l2 |> contains c1
