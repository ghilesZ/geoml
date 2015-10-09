(** This module provides the basic operations over 2D rectangles *)

(** rectangle type *)
type t = Point.t * float * float

(** where p is the bottom-left corner of the rectangle *)
let make p w h : t = (p,w,h)

let bottom_left_corner ((p,_,_):t) = p

let bottom_right_corner ((p,w,_):t) = Point.translate p w 0.

let top_right_corner ((p,w,h):t) = Point.translate p w h

let top_left_corner ((p,_,h):t) = Point.translate p 0. h

let translate ((p,w,h):t) dx dy : t = ((Point.translate p dx dy),w,h) 

let contains ((p,w,h):t) ((px,py):Point.t) = 
  let x=Point.x_coord p and y = Point.y_coord p in
  x < px && px < (x+.w) &&
  y < py && py < y+.h

let area ((_,w,h):t) = w *. h 

let perimeter ((_,w,h):t) = 2. *. (w+.h)

let proj_x r = let open Point in (bottom_left_corner r)//x,(bottom_right_corner r)//x

let proj_y r = let open Point in (bottom_left_corner r)//y,(top_right_corner r)//y

let intersects (s1:t) (s2:t) =
  let (a,b) = proj_x s1 and (c,d) = proj_x s2 in
  if (a<d && b>c) then
    let (a,b) = proj_y s1
    and (c,d) = proj_y s2 in
    (a<d && b>c)
  else false

(** given a rectangle and point, returns the smallest rectangle that
   contains the points and the rectangle given as parameters*)
let encompass (p1,w,h) p2 =
  let l = min (Point.x_coord p1) (Point.x_coord p2)
  and t = max (Point.y_coord p1 +. h) (Point.y_coord p2)
  and r = max (Point.x_coord p1 +. w) (Point.x_coord p2)
  and b = min (Point.y_coord p1) (Point.y_coord p2)
  in ((Point.make l b),r-.l,t-.b)

(** given a list of point, returns the smallest rectangle that
   contains all the points of the list *)
let bounding (pts : Point.t list) : t =
  match pts with
  | [x] -> (x,0.,0.)
  | x::tl -> List.fold_left encompass (x,0.,0.) tl
  | [] -> failwith "can't build a bounding rectangle with an empty list"
