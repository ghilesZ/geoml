type t = float * float

let make x y = (x,y)

let center (a,b) (c,d) = make ((a+.c) /. 2.) ((b+.d) /. 2.) 

let sq_distance (a,b) (c,d) = 
  let diffX = a -. c and diffY = b -. d in
  (diffX *. diffX +. diffY *. diffY)

let distance (a,b) (c,d) = sq_distance (a,b) (c,d) |> sqrt

let x_coord (a,_) = a

let y_coord (_,b) = b

let translate (x,y) dx dy = make (x+.dx) (y+.dy)
