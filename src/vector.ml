type t =
  {
    dx : float ;
    dy : float ;
  }

let print fmt {dx;dy} = Format.fprintf fmt "{dx=%f; dy=%f}" dx dy

let make dx dy = {dx;dy}

let null = make 0. 0.

let x_coord (v:t) = v.dx

let y_coord (v:t) = v.dy

let of_points (a: Point.t) (b: Point.t) : t =
  make (b.Point.x-.a.Point.x) (b.Point.y-.a.Point.y)

let magnitude ({dx;dy}:t) = sqrt (dx*.dx +. dy*.dy)

let magnitude_sq  ({dx;dy}:t) = (dx*.dx +. dy*.dy)

let normalize (({dx;dy} as v):t) =
  let m = magnitude v in
  {dx=dx/.m;dy=dy/.m}

let rotation theta {dx;dy} =
  let s_t = sin theta and c_t = cos theta in
  let x = dx *. c_t -. dy *. s_t and y = dx *. s_t +. dy *. c_t in
  {dx=x; dy=y}

let dot_product ({dx=a;dy=b}:t) ({dx=c;dy=d}:t) = a*.c +. b*.d

let scal_mult f ({dx;dy}:t) : t = make (f*.dx) (f*.dy)

let determinant v1 v2 = v1.dx *. v2.dy -. v1.dy *. v2.dx

let opposite v = scal_mult (-1.) v

let add (v1:t) (v2:t) : t = make (v1.dx+.v2.dx) (v1.dy+.v2.dy)

let substract v1 v2 = opposite v2 |> add v1

let move_to ({dx;dy}:t) = Point.translate dx dy

let projection v1 v2 =
  scal_mult (1. /. magnitude_sq v2) (scal_mult (dot_product v1 v2) v2)

let angle v1 v2 =
  (* let v1 = normalize v1 *)
  (* and v2 = normalize v2 in *)
  (* dot_product v1 v2 |> acos *)
  let sens = make v2.dy (-.v2.dx) in
  let x = dot_product v1 v2 and y = dot_product v1 sens in
  atan2 y x

let angle_deg v1 v2 = 57.2958 *. (angle v1 v2)

(* let reflect v1 v2 = *)
(*   let alpha = angle v1 v2 in *)
(*   rotation (2. *. alpha) v1 *)

let reflect a o =
  let b = projection a o in
  substract (add b b) a
