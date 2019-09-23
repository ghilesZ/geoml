module Make
         (A:Arith.T)
         (P:Signatures.Point_Sig with type arith = A.t)
         (L:Signatures.Line_sig with type point = P.t and type arith = A.t)
         (S:Signatures.Segment_sig with type point = P.t and type line = L.t)
  = struct

  type arith = A.t
  type segment = S.t
  type point = P.t
  type line = L.t

  type t = point * arith * arith

  open A

  let make p w h = (p,w,h)

  let of_diagonal p1 p2 =
    let open P in
    let x,w = if p1.x < p2.x then (p1.x,p2.x-.p1.x) else (p2.x, p1.x-.p2.x)
    and y,h = if p1.y < p2.y then (p1.y,p2.y-.p1.y) else (p2.y, p1.y-.p2.y) in
    ((make x y), w, h)

  let bottom_left_corner (p,_,_) = p

  let bottom_right_corner (p,w,_) = P.translate w zero p

  let top_right_corner (p,w,h) = P.translate w h p

  let top_left_corner (p,_,h) = P.translate zero h p

  let scale_x (p,w,h) f = ((P.scale_x p f),w*.f,h)

  let scale_y (p,w,h) f = ((P.scale_y p f),w,h*.f)

  let scale r f = scale_y (scale_x r f) f

  let translate dx dy (p,w,h) = ((P.translate dx dy p),w,h)

  let point_reflection pivot (p,w,h) =
    let r = make (P.point_reflection pivot p) w h in
    translate (neg w) (neg h) r

  let contains (p,w,h) (pt:point) =
    let open P in
    p.x < pt.x && pt.x < (p.x+.w) &&
      p.y < pt.y && pt.y < (p.y+.h)

  let area (_,w,h) = w *. h

  let perimeter (_,w,h) = two *. (w+.h)

  let proj_x r = let open P in (bottom_left_corner r).x,(bottom_right_corner r).x

  let proj_y r = let open P in (bottom_left_corner r).y,(top_right_corner r).y

  let intersects s1 s2 =
    let (a,b) = proj_x s1 and (c,d) = proj_x s2 in
    if (a<d && b>c) then
      let (a,b) = proj_y s1
      and (c,d) = proj_y s2 in
      (a<d && b>c)
    else false

  let segments r =
    let s1 = S.make (bottom_right_corner r) (bottom_left_corner r)
    and s2 = S.make (bottom_left_corner r) (top_left_corner r)
    and s3 = S.make (top_left_corner r) (top_right_corner r)
    and s4 = S.make (top_right_corner r) (bottom_right_corner r)
    in [s1;s2;s3;s4]

  let is_square (_,w,h) = w=h

  let encompass (p1,w,h) p2 =
    let l = min (P.x_coord p1) (P.x_coord p2)
    and t = max (P.y_coord p1 +. h) (P.y_coord p2)
    and r = max (P.x_coord p1 +. w) (P.x_coord p2)
    and b = min (P.y_coord p1) (P.y_coord p2)
    in ((P.make l b),r-.l,t-.b)

  let bounding (pts : point list) =
    match pts with
    | [x] -> (x,zero,zero)
    | x::tl -> List.fold_left encompass (x,zero,zero) tl
    | [] -> invalid_arg "can't build a bounding rectangle with an empty list"

  let intersect_line r l =
    let inter =
      segments r
      |> List.map (fun e -> S.intersect_line e l)
      |> List.filter (fun e -> e <> None) in
    match inter with
    | [Some a; Some b] -> [a;b]
    | _ -> []

  let centroid r =
    top_right_corner r |> P.center (bottom_left_corner r)

  let random_point (p,w,h) =
    let open P in
    let x = p.x +. ((Random.float (A.to_float w)) |> A.of_float)
    and y = p.y +. ((Random.float (A.to_float h)) |> A.of_float) in
    make x y

  let print fmt (bl,w,h) =
    Format.fprintf fmt "bottom left corner:%a, width=%a, height:%a"
      P.print bl pp_print w pp_print h
end
