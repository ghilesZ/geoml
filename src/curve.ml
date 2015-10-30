module Quadratic = struct 
(** This module provides basic operations over quadratic bezier curves *)
type t = Point.t * Point.t * Point.t

(** Make p0 p1 p2 build a bezier curve starting in p0, 
    and ending in p2, with p1 and as control point *)
let make p0 p1 p2 = (p0,p1,p2)

let start ((p0,_,_):t) = p0

let ending ((_,_,p2):t) = p2

let control ((_,p1,_):t) = p1

(** equation t with t belong to [0.;1.]*)
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

(** equation t with t belong to [0.;1.]*)
let equation (p0,p1,p2,p3) t = 
  let open Point in
  if t < 1. && t > 0. then
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
    while !cur < 1. do
      res := (equation b (!cur))::(!res);
      cur := !cur +. step
    done;
    !res |> List.rev

let points x:t = x
end
