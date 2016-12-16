type t =
  {
    dx : float ;
    dy : float ;
  }

let make dx dy = {dx;dy}

let null = make 0. 0.

let x_coord (v:t) = v.dx

let y_coord (v:t) = v.dy

let of_points (a: Point.t) (b: Point.t) : t =
  make (b.Point.x-.a.Point.x) (b.Point.y-.a.Point.y)

let magnitude ({dx;dy}:t) = sqrt (dx*.dx +. dy*.dy)

let normalize (({dx;dy} as v):t) =
  let m = magnitude v in
  {dx=dx/.m;dy=dy/.m}

let rotation theta {dx;dy} =
  { dx=dx *. cos theta -. dy *. sin theta
  ; dy=dx *. sin theta +. dy *. cos theta }

let dot_product ({dx=a;dy=b}:t) ({dx=c;dy=d}:t) = a*.c +. b*.d

let scal_mult f ({dx;dy}:t) : t = make (f*.dx) (f*.dy)

let determinant v1 v2 = v1.dx *. v2.dy -. v1.dy *. v2.dx

let opposite v = scal_mult (-1.) v

let add (v1:t) (v2:t) : t = make (v1.dx+.v2.dx) (v1.dy+.v2.dy)

let substract v1 v2 = opposite v2 |> add v1

let move_to ({dx;dy}:t) = Point.translate dx dy

let angle v1 v2 =
  let v1 = normalize v1
  and v2 = normalize v2 in
  dot_product v1 v2 |> acos

let angle_deg v1 v2 = 57.2958 *. (angle v1 v2)
