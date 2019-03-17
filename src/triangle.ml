type t = Point.t * Point.t * Point.t

let as_points t = t
let tri_map f ((pa,pb,pc):t) = ((f pa),(f pb),(f pc))

let print fmt (a, b, c) =
  Format.fprintf fmt "(%a, %a, %a)" Point.print a
    Point.print b Point.print c

let tri_exists f (pa,pb,pc) = (f pa) || (f pb) || (f pc)

let tri_find f ((pa,pb,pc):t) =
  if f pa then pa
  else if f pb then pb
  else if f pc then pc
  else raise Not_found

let tri_forall f ((pa,pb,pc):t) = (f pa) && (f pb) && (f pc)

let tri_iter f ((pa,pb,pc):t) = f pa; f pb; f pc

let make p1 p2 p3 : t= (p1,p2,p3)

let extr1 ((p1,_,_):t) = p1

let extr2 ((_,p2,_):t) = p2

let extr3 ((_,_,p3):t) = p3

let scale_x t f =
  tri_map (fun e -> Point.scale_x e f) t

let scale_y t f =
  tri_map (fun e -> Point.scale_y e f) t

let translate dx dy (tr:t) : t =
  tri_map (Point.translate dx dy) tr

let transform t m : t = tri_map (Point.transform m) t

let point_reflection  p (tr:t) : t =
  tri_map (fun e -> Point.point_reflection p e) tr

(** tests if a point is in a triangle with barycentric coordinates *)
let contains (({Point.x=ax;Point.y=ay},
	             {Point.x=bx;Point.y=by},
	             {Point.x=cx;Point.y=cy}):t)
      ({Point.x=px;Point.y=py}:Point.t) =
  let l1 =
    ((by -. cy) *. (px -. cx) +. (cx -. bx) *. (py -. cy)) /.
    ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
  and l2 =
    ((cy -. ay) *. (px -. cx) +. (ax -. cx) *. (py -. cy)) /.
    ((by -. cy) *. (ax -. cx) +. (cx -. bx) *. (ay -. cy))
  in
  let l3 = 1. -. l1 -. l2 in
  l3 > 0. && l3 < 1. && l2 > 0. && l2 < 1. && l1 > 0. && l1 < 1.

let area ((pa,pb,pc):t) =
  let ab = Vector.of_points pa pb and ac = Vector.of_points pa pc
  in Vector.determinant ab ac *. 0.5

let perimeter ((pa,pb,pc):t) =
  Point.distance pa pb +. Point.distance pb pc +. Point.distance pc pa

let proj_x ((pa,pb,pc):t) =
  let open Point in
  let inf = min (pa.x) (pb.x) |> min (pc.x)
  and max = max (pa.x) (pb.x) |> max (pc.x)
  in inf,max

let proj_y ((pa,pb,pc):t) =
  let open Point in
  let inf = min (pa.y) (pb.y) |> min (pc.y)
  and max = max (pa.y) (pb.y) |> max (pc.y)
  in (inf,max)

let segments ((pa,pb,pc):t) =
  ((Segment.make pa pb),(Segment.make pb pc),(Segment.make pc pa))

let intersects (s1:t) (s2:t) =
  let (a,b,c) = segments s1 and (d,e,f) = segments s2 in
   tri_exists (fun s ->
     Segment.intersects d s ||
     Segment.intersects e s ||
     Segment.intersects f s
   ) (a,b,c)

let intersect_line (t:t) (l:Line.t) =
  let (a,b,c) = segments t in
  List.fold_left (fun acc s ->
    match  Segment.intersect_line s l with
    | None -> acc
    | Some p -> p::acc
  ) [] [a;b;c]

let is_isoscele ((a,b,c) :t) =
  Point.sq_distance a b = Point.sq_distance b c ||
  Point.sq_distance b c = Point.sq_distance a c ||
  Point.sq_distance a b = Point.sq_distance a c

let is_equilateral ((a,b,c) :t) =
  Point.sq_distance a b = Point.sq_distance b c &&
  Point.sq_distance b c = Point.sq_distance a c

let is_right ((a,b,c) :t) =
  let psq = Point.sq_distance in
  psq a b = psq b c +. psq c a ||
  psq b c = psq a b +. psq c a ||
  psq c a = psq a b +. psq b c

let points x = x

let angles ((pa,pb,pc):t) =
  let open Vector in
  let a1 = angle_deg (of_points pa pb) (of_points pa pc) in
  let a2 = angle_deg (of_points pb pa) (of_points pb pc) in
  let a1 = abs_float a1 and a2 = abs_float a2 in
  let a3 = 180. -. (a1+.a2) in
  (a1,a2,a3)

let centroid ((a,b,c):t) =
  let bc = Point.center b c and ac = Point.center a c in
  let al = Line.of_points bc a and bl = Line.of_points ac b in
  Line.intersection bl al

let random_point ((a,b,c):t) : Point.t =
  let ab = Vector.of_points a b and ac = Vector.of_points a c in
  let randab = Vector.scal_mult (Random.float 1.) ab
  and randac = Vector.scal_mult (Random.float 1.) ac in
  let p = Vector.move_to (Vector.add randab randac) a in
  let bc = Vector.of_points b c and bp = Vector.of_points b p in
  if (Vector.determinant bc bp) *. Vector.determinant bc ab < 0. then p
  else Point.point_reflection (Point.center b c) p
