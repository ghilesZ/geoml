type t = Point.t * Point.t * Point.t

let make p1 p2 p3 : t= (p1,p2,p3)

(* tests if a point is in a triangle with barycenter method *)
let contains (((ax,ay), (bx,by), (cx,cy)):t) ((px,py):Point.t) = 
  let l1 = 
    ((by -. cy) *. (px -. cx) +. (cx -. bx) *. (py -. cy)) /.
    ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
  and l2 = 
    ((cy -. ay) *. (px -. cx) +. (ax -. cx) *. (py -. cy)) /.
    ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
  in 
  let l3 = 1. -. l1 -. l2 in
  l3 > 0. && l3 < 1. && l2 > 0. && l2 < 1. && l1 > 0. && l1 < 1.

let area ((pa,pb,pc):t) = 
  let a = Point.distance pa pb
  and b = Point.distance pb pc
  and c = Point.distance pc pa in
  let s = 0.5 *. (a+.b+.c) in 
  sqrt (s *. (s-.a) *. (s-.b) *.(s-.c))

let perimeter ((pa,pb,pc):t) =   
  Point.distance pa pb +. Point.distance pb pc +. Point.distance pc pa 

(*let proj_x ((pa,pb,pc):t) =
  let open Point in
  (min (min pa//x pb//x) pc//x),(max (max pa//x pb//x) pc//x)

let proj_y ((pa,pb,pc):t) =
  let open Point in  
  (min (min pa//y pb//y) pc//y),(max (max pa//y pb//y) pc//y)*)

