type t = Point.t * Point.t * Point.t

let make p1 p2 p3 = (p1,p2,p3)

(* tests if a point is in a triangle with barycenter method *)
let contains ((ax,ay), (bx,by), (cx,cy)) (px,py) = 
  let l1 = 
    ((by -. cy) *. (px -. cx) +. (cx -. bx) *. (py -. cy)) /.
    ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
  and l2 = 
    ((cy -. ay) *. (px -. cx) +. (ax -. cx) *. (py -. cy)) /.
    ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
  in 
  let l3 = 1. -. l1 -. l2 in
  l3 > 0. && l3 < 1. && l2 > 0. && l2 < 1. && l1 > 0. && l1 < 1.

let area (pa,pb,pc) = 
  let a = Point.distance pa pb 
  and b = Point.distance pb pc
  and c = Point.distance pc pa in
  let s = 0.5 *. (a+.b+.c) in 
  sqrt (s *. (s-.a) *. (s-.b) *.(s-.c))

let perimeter (pa,pb,pc) =   
  Point.distance pa pb +. Point.distance pb pc +. Point.distance pc pa 
