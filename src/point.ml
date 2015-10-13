type t =
  { x : float ;
    y : float ;
  }

let make x y : t = {x; y}

let center ({x=a;y=b}: t) ({x=c;y=d} : t) : t = make ((a+.c) /. 2.) ((b+.d) /. 2.) 

let sq_distance ({x=a;y=b}: t) ({x=c;y=d}: t) = 
  let diffX = a -. c and diffY = b -. d in
  (diffX *. diffX +. diffY *. diffY)

let distance ({x=a;y=b}: t) ({x=c;y=d}: t) = sq_distance {x=a;y=b} {x=c;y=d} |> sqrt

let x_coord (p:t) = p.x

let y_coord (p:t) = p.y

let translate ({x;y}: t) dx dy : t= make (x+.dx) (y+.dy)

(* SHORTCUT *)
type coordinate = Xc | Yc
let x = Xc
let y = Yc


