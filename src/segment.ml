module Make
         (A:Arith.T)
         (P:Signatures.Point_Sig with type arith = A.t)
         (L:Signatures.Line_sig with type point = P.t) = struct

  type arith = A.t
  type affine = P.affine
  type point = P.t
  type line = L.t

  type t = point * point

  let make (p1:point) (p2:point) : t = (p1,p2)

  let extr1 ((p1,_):t) : point = p1

  let extr2 ((_,p2):t) : point = p2

  let sq_size ((p1,p2):t) = P.sq_distance p1 p2

  let size ((p1,p2):t) = P.distance p1 p2

  let map f (p1, p2) = (f p1, f p2)

  let center (p1,p2) = P.center p1 p2

  open A

  let equation (p1,p2) t =
    if t < zero || t > one then
      invalid_arg "Segment.equation: parameter must be in [0. ; 1.]"
    else
      let open P in
      let (dx,dy) = (p2.x -. p1.x),(p2.y -. p1.y) in
      P.translate (t*.dx) (t*.dy) p1

  let scale_y (p1,p2) f = make (P.scale_y p1 f) (P.scale_y p2 f)

  let scale_x (p1, p2) f = make (P.scale_x p1 f) (P.scale_x p2 f)

  let translate dx dy = map (P.translate dx dy)

  let transform m = map P.(transform m)

  let to_line (p1,p2) = L.of_points p1 p2

  let contains (a,b) p =
    P.sq_distance a p +. P.sq_distance p b =  P.sq_distance a b

  let proj_x (a,b) =
    let open P in
    if a.x > b.x then b.x,a.x
    else a.x,b.x

  let proj_y (a,b) =
    let open P in
    if a.y > b.y then b.y,a.y
    else a.y,b.y

  let intersects (a1, b1 as s1) s2 =
    let open P in
    try
      let p = L.intersection (to_line s1) (to_line s2)
      and sqd = sq_distance a1 b1 in
      sq_distance a1 p <= sqd && sq_distance b1 p <= sqd
    with
    | L.(Error(Parallel(_))) -> false

  let intersection ((a1, b1 as s1)) s2 =
    let open P in
    try
      let p = L.intersection (to_line s1) (to_line s2)
      and sqd = sq_distance a1 b1 in
      if sq_distance a1 p < sqd && sq_distance b1 p < sqd
      then Some p else None
    with
    | L.(Error (Parallel(_))) -> None

  let intersect_line ((p1,p2) as s) l =
    let open P in
    try
      let p = L.intersection l (to_line s)
      and sqd = sq_distance p1 p2 in
      if sq_distance p1 p <= sqd && sq_distance p2 p <= sqd
      then Some p else None
    with
    | L.(Error(Parallel(_))) -> None
end
