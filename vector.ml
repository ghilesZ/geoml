type t = float * float

let make dx dy = (dx,dy)

let of_points (ax,ay) (bx,by) = (bx-.ax),(by-.ay)

let dot_product (a,b) (c,d) = a*.c +. b*.d


