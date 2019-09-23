module Make
         (A:Arith.T)
         (Aff:Signatures.Affine_Sig)
         (P:Signatures.Point_Sig   with type arith = A.t and type affine = Aff.t)
         (V:Signatures.Vector_sig  with type arith = A.t and type point = P.t)
         (L:Signatures.Line_sig    with type arith = A.t and type point = P.t)
         (S:Signatures.Segment_sig with type point = P.t and type line = L.t)
  = struct

  type arith = A.t
  type segment = S.t
  type point = P.t
  type line = L.t
  type affine = Aff.t

  open A

  type t = point * point * point

  let as_points t = t
  let tri_map f ((pa,pb,pc):t) = ((f pa),(f pb),(f pc))

  let print fmt ((a, b, c):t) =
    Format.fprintf fmt "(%a, %a, %a)" P.print a
      P.print b P.print c

  let tri_exists f (pa,pb,pc) = (f pa) || (f pb) || (f pc)

  let tri_find f ((pa,pb,pc):t) =
    if f pa then pa
    else if f pb then pb
    else if f pc then pc
    else raise Not_found

  let tri_forall f ((pa,pb,pc):t) = (f pa) && (f pb) && (f pc)

  let tri_iter f ((pa,pb,pc):t) = f pa; f pb; f pc

  let make p1 p2 p3 =
    if p1 = p2 || p2 = p3 || p3 = p1
    then invalid_arg "Triange.make: points should be all different"
    else (p1,p2,p3)

  let extr1 ((p1,_,_):t) = p1

  let extr2 ((_,p2,_):t) = p2

  let extr3 ((_,_,p3):t) = p3

  let scale_x t f : t =
    tri_map (fun e -> P.scale_x e f) t

  let scale_y t f : t =
    tri_map (fun e -> P.scale_y e f) t

  let translate dx dy (tr:t) : t =
    tri_map (P.translate dx dy) tr

  let transform t m : t = tri_map (P.transform m) t

  let point_reflection  p (tr:t) : t =
    tri_map (fun e -> P.point_reflection p e) tr

  (* tests if a point is in a triangle with barycentric coordinates *)
  let contains (({P.x=ax;P.y=ay},
	               {P.x=bx;P.y=by},
	               {P.x=cx;P.y=cy}):t)
        ({P.x=px;P.y=py}:point) =
    let l1 =
      ((by -. cy) *. (px -. cx) +. (cx -. bx) *. (py -. cy)) /.
        ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
    and l2 =
      ((cy -. ay) *. (px -. cx) +. (ax -. cx) *. (py -. cy)) /.
        ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
    in
    let l3 = one -. l1 -. l2 in
    l3 > zero && l3 < one && l2 > zero && l2 < one && l1 > zero && l1 < one

  let area ((pa,pb,pc):t) =
    let ab = V.of_points pa pb and ac = V.of_points pa pc
    in V.determinant ab ac *. A.of_float (0.5)

  let perimeter ((pa,pb,pc):t) =
    P.distance pa pb +. P.distance pb pc +. P.distance pc pa

  let proj_x ((pa,pb,pc):t) =
    let inf = min (pa.x) (pb.x) |> min (pc.x)
    and max = max (pa.x) (pb.x) |> max (pc.x)
    in inf,max

  let proj_y ((pa,pb,pc):t) =
    let inf = min (pa.y) (pb.y) |> min (pc.y)
    and max = max (pa.y) (pb.y) |> max (pc.y)
    in (inf,max)

  let segments ((pa,pb,pc):t) =
    ((S.make pa pb),(S.make pb pc),(S.make pc pa))

  let intersects (s1:t) (s2:t) =
    let (a,b,c) = segments s1 and (d,e,f) = segments s2 in
    tri_exists (fun s ->
        S.intersects d s ||
          S.intersects e s ||
            S.intersects f s
      ) (a,b,c)

  let intersect_line (t:t) (l:L.t) =
    let (a,b,c) = segments t in
    List.fold_left (fun acc s ->
        match  S.intersect_line s l with
        | None -> acc
        | Some p -> p::acc
      ) [] [a;b;c]

  let is_isoscele ((a,b,c) :t) =
    P.sq_distance a b = P.sq_distance b c ||
      P.sq_distance b c = P.sq_distance a c ||
        P.sq_distance a b = P.sq_distance a c

  let is_equilateral ((a,b,c) :t) =
    P.sq_distance a b = P.sq_distance b c &&
      P.sq_distance b c = P.sq_distance a c

  let is_right ((a,b,c) :t) =
    let psq = P.sq_distance in
    psq a b = psq b c +. psq c a ||
      psq b c = psq a b +. psq c a ||
        psq c a = psq a b +. psq b c

  let points x = x

  let angles ((pa,pb,pc):t) =
    let open V in
    let a1 = angle_deg (of_points pa pb) (of_points pa pc) in
    let a2 = angle_deg (of_points pb pa) (of_points pb pc) in
    let a1 = if a1 < zero then neg a1 else a1
    and a2 = if a2 < zero then neg a2 else a2 in
    let a3 = A.of_float 180. -. (a1+.a2) in
    (a1,a2,a3)

  let centroid ((a,b,c):t) =
    let bc = P.center b c and ac = P.center a c in
    let al = L.of_points bc a and bl = L.of_points ac b in
    L.intersection bl al

  let random_point (a,b,c) : point =
    let ab = V.of_points a b and ac = V.of_points a c in
    let randab = V.scal_mult (Random.float 1. |> A.of_float)  ab
    and randac = V.scal_mult (Random.float 1. |> A.of_float) ac in
    let p = V.move_to (V.add randab randac) a in
    let bc = V.of_points b c and bp = V.of_points b p in
    if (V.determinant bc bp) *. V.determinant bc ab < zero then p
    else P.point_reflection (P.center b c) p

end
