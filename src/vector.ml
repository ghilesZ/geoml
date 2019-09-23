module Make
         (A:Arith.T)
         (P:Signatures.Point_Sig with type arith = A.t) = struct

  type arith = A.t
  type point = P.t

  type t =
    {
      dx : arith ;
      dy : arith ;
    }

  let print fmt {dx;dy} =
    Format.fprintf fmt "{dx=%a; dy=%a}"
      A.pp_print dx
      A.pp_print dy

  let make dx dy : t = {dx;dy}

  let null = make A.zero A.zero

  let x_coord (v:t) = v.dx

  let y_coord (v:t) = v.dy

  open A

  let of_points (a: point) (b: point) =
    make (b.P.x -. a.P.x) (b.P.y -. a.P.y)

  let magnitude ({dx;dy}) = sqrt (A.to_float (dx*.dx +. dy*.dy)) |> A.of_float

  let magnitude_sq  ({dx;dy}) = (dx*.dx +. dy*.dy)

  let normalize (({dx;dy} as v)) =
    let m = magnitude v in
    {dx=dx/.m;dy=dy/.m}

  let rotation angle {dx;dy} =
    let angle = A.to_float angle in
    let ca = cos angle |> A.of_float in
    let sa = sin angle |> A.of_float in
    let x = dx *. ca -. dy *. sa and y = dx *. sa +. dy *. ca in
    {dx=x; dy=y}

  let dot_product v1 v2 = v1.dx *. v2.dx +. v1.dy *. v2.dy

  let determinant v1 v2 = v1.dx *. v2.dy -. v1.dy *. v2.dx

  let scal_mult f {dx;dy} = make (f*.dx) (f*.dy)

  let opposite {dx;dy} =  make (neg dx) (neg dy)

  let add v1 v2 = make (v1.dx+.v2.dx) (v1.dy+.v2.dy)

  let substract v1 v2 = opposite v2 |> add v1

  let move_to {dx;dy} = P.translate dx dy

  let projection v1 v2 =
    scal_mult (one /. magnitude_sq v2) (scal_mult (dot_product v1 v2) v2)

  let angle v1 v2 =
    let sens = make v2.dy (neg v2.dx) in
    let x = dot_product v1 v2 and y = dot_product v1 sens in
    atan2 (A.to_float y)  (A.to_float x) |>  A.of_float

  let angle_deg v1 v2 = A.of_float 57.2958 *. (angle v1 v2)

  let reflect a o =
    let b = projection a o in
    substract (add b b) a

end
