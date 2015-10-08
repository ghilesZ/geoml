type t = float * float

let make x y : t = (x,y)

let center ((a,b): t) ((c,d) : t) : t = make ((a+.c) /. 2.) ((b+.d) /. 2.) 

let sq_distance ((a,b): t) ((c,d): t) = 
  let diffX = a -. c and diffY = b -. d in
  (diffX *. diffX +. diffY *. diffY)

let distance ((a,b): t) ((c,d): t) = sq_distance (a,b) (c,d) |> sqrt

let x_coord (p:t) = fst p

let y_coord (p:t) = snd p

let translate ((x,y): t) dx dy : t= make (x+.dx) (y+.dy)

(* SHORTCUT *)
type coordinate = Xc | Yc
let x = Xc
let y = Yc
let ( // ) (p:t) x_or_y = match x_or_y with | Xc -> fst p | _ -> snd p
(* this allows to write p$x (resp. p$y) to get the x (resp. y) coordinate *)
