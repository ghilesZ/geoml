type t = float * float

let make dx dy = (dx,dy)

let x_coord ((dx,_):t) = dx

let y_coord ((_,dy):t) = dy

let of_points (ax,ay) (bx,by) = (bx-.ax),(by-.ay)

let magnitude ((dx,dy):t) = sqrt (dx*.dx +. dy*.dy)

let dot_product ((a,b):t) ((c,d):t) = a*.c +. b*.d

let scal_mult ((dx,dy):t) f : t = (f*.dx),(f*.dy)

let opposite v = scal_mult v (-1.)

let add ((dx1,dy1):t) ((dx2,dy2):t) :t = (dx1+.dx2),(dy1+.dy2) 

let substract v1 v2 = opposite v2 |> add v1

let move_to ((dx,dy):t) p = Point.translate p dx dy

let angle v1 v2 = 
  (dot_product v1 v2 |> acos) /. ((magnitude v1) *. (magnitude v2))

let angle_deg v1 v2 = 57.2958 *. (angle v1 v2)
  
