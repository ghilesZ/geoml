type t = Point.t * float * float (* bottom left corner, width, height *)

let make p w h = (p,w,h)

let contains ((x,y),w,h) (px,py) = 
  x < px && px < (x+.w) &&
  y < py && py < y+.h

let area (_,w,h) = w *. h 

let permiter (_,w,h) = 2. *. (w+.h)
