module Quadratic = struct 
(** This module provides basic operations over quadratic bezier curves *)
type t = Point.t * Point.t * Point.t

(** Make p0 p1 p2 build a bezier curve starting in p0, 
    and ending in p2, with p1 and as control point *)
let make p0 p1 p2 = (p0,p1,p2)

let start ((p0,_,_):t) = p0

let ending ((_,_,p2):t) = p2

let control ((_,p1,_):t) = p1

(** equation b t returns the value of the bezier equation b(t)
    with t belong to [0.;1.]*)
let equation (p0,p1,p2) t = 
  let b = 
    let a = (1.-.t)*.(1.-.t) 
    and b = 2.*.t *.(1.-.t)
    and c = t*.t in
    fun c1 c2 c3 -> a *. c1 +. b *. c2 +. c *. c3
  in
  let open Point in
  if t <= 1. && t >= 0. then
    let x = b p0.x p1.x p2.x
    and y = b p0.y p1.y p2.y
    in make x y
  else failwith "Curve.equation: t must be in [0. ; 1.]"

(** points b nb returns a list of nb points uniformly distributed
    on the bezier curve b.*)
let points b nb = 
  if nb <= 0 then [] else
    let res = ref [] and cur = ref 0.
    and step = 1. /. (float_of_int nb) in
    while !cur <= 1. do
      res := (equation b (!cur))::(!res);
      cur := !cur +. step
    done;
    List.rev (!res)
      
(** returns the list of curves defined by the given points*)
let of_points pts = 
  let rec aux res pts =
    match pts with
    | x::y::z::tl -> aux ((make x y z)::res) (z::tl)
    | x::y::tl -> List.rev ((make x (Point.center x y) y)::res)
    | x::tl -> List.rev ((make x x x)::res)
    | [] -> List.rev res 
  in aux [] pts
end

module Cubic = struct 
(** This module provides basic operations over cubic bezier curves *)
type t = Point.t * Point.t * Point.t * Point.t

(** Make p0 p1 p2 p3 build a bezier curve starting in p0, 
    and ending in p3, with p1 and p2 as control points *)
let make p0 p1 p2 p3 = (p0,p1,p2,p3)

let start ((p0,_,_,_):t) = p0

let ending ((_,_,_,p3):t) = p3
  
(** equation t with t belong to [0.;1.]*)
let equation (p0,p1,p2,p3) t = 
  let open Point in
  if t <= 1. && t >= 0. then
    let m_t = (1. -. t) *. (1. -. t)
    and t_2 = t*.t in
    let x = 
      p0.x *. m_t *. (1. -. t) +.
      3. *. p1.x *. t *. m_t +.
      3. *. p2.x *. t_2 *. (1. -. t) +. 
      p3.x *. t_2 *. t
    and y = 
      p0.y *. m_t *. (1. -. t) +.
      3. *. p1.y *. t *. m_t +.
      3. *. p2.y *. t_2 *. (1. -. t) +. 
      p3.y *. t_2 *. t
    in make x y
  else failwith "Curve.equation: t must be in [0. ; 1.]"

(** points b nb returns a list of nb points uniformly distributed
    on the bezier curve b.*)
let points b nb = 
  if nb <= 0 then [] else
    let res = ref [] and cur = ref 0.
    and step = 1. /. (float_of_int nb) in
    while !cur <= 1. do
      res := (equation b (!cur))::(!res);
      cur := !cur +. step
    done;
    !res |> List.rev
end

module BSpline = struct 
(** This module provides basic operations over B-spline curves *)
  type t = {degre:int; knots:float array; nb:int; control: Point.t list}
    
  (** make pts k, makes a B-spline 
      with pts the control points and k the knots Array*)
  let make pts k =
    let nb_knots = Array.length k in
    let m = nb_knots - 1 in
    let d = m - (List.length pts)
    in 
    {degre=d; knots=k; nb=nb_knots; control = pts}

  (** make_eq pts nb_knots, makes a cardinal B-spline 
     with a constant separation, 1/(nb_knots-1), between knots*)    
  let make_eq pts nb_knots =
    let step = 1. /. (float_of_int (nb_knots-1))
    and tmp = ref 0.
    in
    let next () =
      let res = !tmp in
      tmp:=!tmp +. step;
      res
    in
    let k = Array.make nb_knots 0.
	   |> Array.map (fun _ -> next())
    in
    Array.iter (fun e -> print_float e; print_newline()) k;
    make pts k

  let start {control} = List.hd control

  let ending {control} = Common.List.last control
      
  let equation ({degre;knots;nb;control} as c) t =
    if t < 0. || t > 1. then failwith "t must be in [0. ; 1.]"
    else if t = 0. then start c
    else if t = 1. then ending c else
      let rec bspl cur deg t =
      (*print_int cur;
	print_newline();*)
	match deg with
	| 1 ->
	 (*print_endline "0";*)
	   if knots.(cur) <= t  && t <= knots.(cur+1)  then 1. else 0.
	| _ ->
	 (*print_endline "else";*)
	   let coeff1 = (t -. knots.(cur)) /. (knots.(cur + deg -1) -. knots.(cur))
	   and coeff2 = (knots.(cur+deg) -. t) /.(knots.(cur+deg)-.knots.(cur+1))
	   in
	   coeff1 *. (bspl cur (deg - 1) t) +. coeff2 *. (bspl (cur+1) (deg-1) t)
      in
      let rec sum accu i n p tail t =
	if i = n then accu +. (bspl i n t) *. p
	else
	  let accu = accu +. (bspl i n t) *. p in
	  sum accu (i+1) n (List.hd tail) (List.tl tail) t
      in
      let xs = (List.map Point.x_coord control)
      and ys = (List.map Point.y_coord control) in
      let x = sum 0. 0 ((nb - degre) - 2) (List.hd xs) (List.tl xs) t
      and y = sum 0. 0 ((nb - degre) - 2) (List.hd ys) (List.tl ys) t
      in Point.make x y
      
  let points b nb = 
    if nb <= 0 then [] else
      let res = ref [] and cur = ref 0.
      and step = 1. /. (float_of_int nb) in
      while !cur <= 1. do
	res := (equation b (!cur))::(!res);
	cur := !cur +. step
      done;
    !res |> List.rev
    
end
