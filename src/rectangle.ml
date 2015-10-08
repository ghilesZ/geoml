type t = Point.t * float * float (* bottom left corner, width, height *)

(* where p is the bottom-left corner of the rectangle *)
let make p w h : t = (p,w,h)

let bottom_left_corner ((p,_,_):t) = p

let bottom_right_corner ((p,w,_):t) = Point.translate p w 0.

let top_right_corner ((p,w,h):t) = Point.translate p w h

let top_left_corner ((p,_,h):t) = Point.translate p 0. h

let contains ((p,w,h):t) ((px,py):Point.t) = 
  let x=Point.x_coord p and y = Point.y_coord p in
  x < px && px < (x+.w) &&
  y < py && py < y+.h

let area ((_,w,h):t) = w *. h 

let permiter ((_,w,h):t) = 2. *. (w+.h)

let proj_x = failwith "todo"

let proj_y = failwith "todo"

let intersects (r1:t) (r2:t) = failwith "todo"


