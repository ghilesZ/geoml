type t = Point.t * Point.t * Point.t

let of_points = Fun.id

let tri_map f ((pa, pb, pc) : t) = (f pa, f pb, f pc)

let print fmt (a, b, c) =
  Format.fprintf fmt "(%a, %a, %a)" Point.print a Point.print b Point.print c

let tri_exists f (pa, pb, pc) = f pa || f pb || f pc

let tri_find f ((pa, pb, pc) : t) =
  if f pa then pa
  else if f pb then pb
  else if f pc then pc
  else raise Not_found

let tri_forall f ((pa, pb, pc) : t) = f pa && f pb && f pc

let tri_iter f ((pa, pb, pc) : t) = f pa ; f pb ; f pc

let make p1 p2 p3 : t = (p1, p2, p3)

let extr1 ((p1, _, _) : t) = p1

let extr2 ((_, p2, _) : t) = p2

let extr3 ((_, _, p3) : t) = p3

let scale_x t f = tri_map (fun e -> Point.scale_x e f) t

let scale_y t f = tri_map (fun e -> Point.scale_y e f) t

let translate dx dy (tr : t) : t = tri_map (Point.translate dx dy) tr

let transform t m : t = tri_map (Point.transform m) t

let rotate (tr : t) p alpha : t =
  tri_map (fun p' -> Point.rotate p p' alpha) tr

let rotate_angle (tr : t) p alpha : t =
  tri_map (fun p' -> Point.rotate_angle p p' alpha) tr

let reflection p (tr : t) : t = tri_map (Point.reflection p) tr

(* tests if a point is in a triangle with barycentric coordinates *)
let contains (Point.({x= ax; y= ay}, {x= bx; y= by}, {x= cx; y= cy}) : t)
    Point.{x= px; y= py} =
  let bcy = by -. cy in
  let cbx = cx -. bx in
  let acx = ax -. cx in
  let acy = ay -. cy in
  let pcx = px -. cx in
  let pcy = py -. cy in
  let den = (bcy *. acx) +. (cbx *. acy) in
  let l1 = ((bcy *. pcx) +. (cbx *. pcy)) /. den
  and l2 = (((cy -. ay) *. pcx) +. (acx *. pcy)) /. den in
  let l3 = 1. -. l1 -. l2 in
  l3 > 0. && l3 < 1. && l2 > 0. && l2 < 1. && l1 > 0. && l1 < 1.

let area ((pa, pb, pc) : t) =
  let ab = Vector.of_points pa pb and ac = Vector.of_points pa pc in
  Vector.determinant ab ac *. 0.5

let perimeter ((pa, pb, pc) : t) =
  Point.distance pa pb +. Point.distance pb pc +. Point.distance pc pa

let proj_x ((pa, pb, pc) : t) =
  let open Point in
  let inf = min pa.x pb.x |> min pc.x and max = max pa.x pb.x |> max pc.x in
  (inf, max)

let proj_y ((pa, pb, pc) : t) =
  let open Point in
  let inf = min pa.y pb.y |> min pc.y and max = max pa.y pb.y |> max pc.y in
  (inf, max)

let segments ((pa, pb, pc) : t) =
  (Segment.make pa pb, Segment.make pb pc, Segment.make pc pa)

let intersects (s1 : t) (s2 : t) =
  let a, b, c = segments s1 and d, e, f = segments s2 in
  tri_exists
    (fun s ->
      Segment.intersects d s || Segment.intersects e s
      || Segment.intersects f s)
    (a, b, c)

let intersect_line (t : t) (l : Line.t) =
  let a, b, c = segments t in
  List.fold_left
    (fun acc s ->
      match Segment.intersect_line s l with
      | None -> acc
      | Some p -> p :: acc)
    [] [a; b; c]

let is_isoscele ((a, b, c) : t) =
  Point.sq_distance a b = Point.sq_distance b c
  || Point.sq_distance b c = Point.sq_distance a c
  || Point.sq_distance a b = Point.sq_distance a c

let is_equilateral ((a, b, c) : t) =
  Point.sq_distance a b = Point.sq_distance b c
  && Point.sq_distance b c = Point.sq_distance a c

let is_right ((a, b, c) : t) =
  let psq = Point.sq_distance in
  psq a b = psq b c +. psq c a
  || psq b c = psq a b +. psq c a
  || psq c a = psq a b +. psq b c

let points x = x

let angles ((pa, pb, pc) : t) =
  let open Vector in
  let a1 = angle_deg (of_points pa pb) (of_points pa pc) in
  let a2 = angle_deg (of_points pb pa) (of_points pb pc) in
  let a1 = abs_float a1 and a2 = abs_float a2 in
  let a3 = 180. -. (a1 +. a2) in
  (a1, a2, a3)

let centroid ((a, b, c) : t) =
  let bc = Point.center b c and ac = Point.center a c in
  let al = Line.of_points bc a and bl = Line.of_points ac b in
  Line.intersection bl al

let random_point st ((a, b, c) : t) : Point.t =
  let ab = Vector.of_points a b and ac = Vector.of_points a c in
  let r1 = Random.State.float st 1. in
  let r2 = Random.State.float st 1. in
  let r1, r2 = if r1 +. r2 <= 1. then (r1, r2) else (1. -. r1, 1. -. r2) in
  Vector.(move_to (add (scal_mult r1 ab) (scal_mult r2 ac))) a
