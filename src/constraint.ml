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
  let open Line in
  let (a,b,c) = Line.get_coeff l in
  Format.fprintf fmt "%fx + %fy + %f %s 0" a b c (comp_to_string cmp)

let make l comp : t =
  (l,comp)

let get_border (l,_) = l

let get_comp (_,c) = c

let contains ((l,comp):t) p =
  let (a,b,c) = Line.get_coeff l in
  let value =
    let open Point in
    a *. p.x +. b *. p.y +. c
  in
  let res = (compf comp) value 0. in
  (* if res then Format.printf "point %a statisfies %a\n%!" Point.print p print cstr *)
  (* else Format.printf "point %a does not statisfies %a\n%!" Point.print p print cstr; *)
  res

(**contains c p returns true if the point p is in the half-space defined by c*)
let translate dx dy ((l,comp):t) =
  make (Line.translate dx dy l) comp

let complementary (l,comp) = l,(neg comp)

let intersects (((l1,comp1)as c1):t) (((l2,comp2) as c2) :t) =
  match comp1,comp2 with
  | x,y when Line.parallel l1 l2 ->
     let v1 = Line.arbitrary_point l1 in
     contains c2 v1 ||
       (let v2 = Line.arbitrary_point l2 in
	contains c1 v2)
  | _ -> true
