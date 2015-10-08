type t = Point.t * Point.t

let make p1 p2 = (p1,p2)

let extr1 (p1,_) = p1

let extr2 (_,p2) = p2
 
let size (p1,p2) = Point.distance p1 p2
