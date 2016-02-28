type t =
  {
    x : float ;
    y : float ;
  }

let make x y : t = {x; y}

type point = t

module Tbl = Hashtbl.Make (struct
    type t = point
    let equal = (==)
    let hash = Hashtbl.hash
  end)

 
let orig = make 0. 0.

let center {x;y} {x=a;y=b} = make ((a+.x) /. 2.) ((b+.y) /. 2.)

let determinant a b c =
  (b.x -. a.x) *. (c.y -. a.y) -. (b.y -. a.y) *. (c.x -. a.x)
    
let iso_barycenter pts =
  let rec aux pts sumx sumy nb = 
    match pts with 
    | [] -> make (sumx /. nb) (sumy /. nb)
    | h::tl -> aux tl (sumx +. h.x) (sumy +. h.y) (nb +. 1.)
  in aux pts 0. 0. 0.

let barycenter weighted_pts =
  let rec aux pts sumx sumy sumw =
    match pts with
    | [] -> make (sumx /. sumw) (sumy /. sumw)
    | (pt,w)::tl -> aux tl ((w*.pt.x) +. sumx) ((w*.pt.y) +. sumy) (w+.sumw)
  in aux weighted_pts 0. 0. 0.
  
let sq_distance ({x=a;y=b}: t) ({x=c;y=d}: t) = 
  let diffX = a -. c and diffY = b -. d in
  (diffX *. diffX +. diffY *. diffY)

let distance ({x=a;y=b}: t) ({x=c;y=d}: t) = sq_distance {x=a;y=b} {x=c;y=d} |> sqrt

let x_coord (p:t) = p.x

let y_coord (p:t) = p.y

let scale_x (p:t) f = {p with x=p.x*.f}

let scale_y (p:t) f = {p with y=p.y*.f}

let translate dx dy ({x;y}: t) : t= make (x+.dx) (y+.dy)

let transform aff p =
  let x, y = Affine.transform_point aff p.x p.y in {x; y}

let point_reflection center p =
  translate (center.x -. p.x) (center.y -. p.y) center 

let rotate pivot p angle =
  let px = (cos angle) *. (p.x -. pivot.x) -. (sin angle) *. (p.y -. pivot.y) +. pivot.x  
  and py = (sin angle) *. (p.x -. pivot.x) +. (cos angle) *. (p.y -. pivot.y) +. pivot.y
  in make px py

let rotate_angle pivot p angle = rotate pivot p (angle *. Math.deg_to_rad)

let print fmt pt = Format.fprintf fmt "{x=%f; y=%f}" pt.x pt.y
