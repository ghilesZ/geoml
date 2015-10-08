type t = Point.t * float * float (* bottom left corner, width, height *)

let make p w h = (p,w,h)

let bottom_left_corner (p,_,_) = p

let bottom_right_corner (p,w,_) = Point.translate p w 0.

let top_right_corner (p,w,h) = Point.translate p w h

let top_left_corner (p,_,h) = Point.translate p 0. h

let contains ((x,y),w,h) (px,py) = 
  x < px && px < (x+.w) &&
  y < py && py < y+.h

let area (_,w,h) = w *. h 

let permiter (_,w,h) = 2. *. (w+.h)

let intersects r1 r2 = failwith "todo" (*
   bottom_left_corner r1 |> contains r2
|| bottom_right_corner r1 |> contains r2
|| top_right_corner r1 |> contains r2
|| top_left_corner r1 |> contains r2
		       *)

  


