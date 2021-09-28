type t = Point.t * float * float

let make p w h : t = (p, w, h)

let of_diagonal p1 p2 : t =
  let open Point in
  let x, w =
    if p1.x < p2.x then (p1.x, p2.x -. p1.x) else (p2.x, p1.x -. p2.x)
  and y, h =
    if p1.y < p2.y then (p1.y, p2.y -. p1.y) else (p2.y, p1.y -. p2.y)
  in
  (make x y, w, h)

let bottom_left_corner ((p, _, _) : t) = p

let bottom_right_corner ((p, w, _) : t) = Point.translate w 0. p

let top_right_corner ((p, w, h) : t) = Point.translate w h p

let top_left_corner ((p, _, h) : t) = Point.translate 0. h p

let scale_x ((p, w, h) : t) f = (Point.scale_x p f, w *. f, h)

let scale_y ((p, w, h) : t) f = (Point.scale_y p f, w, h *. f)

let scale r f = scale_y (scale_x r f) f

let translate dx dy ((p, w, h) : t) : t = (Point.translate dx dy p, w, h)

let point_reflection pivot ((p, w, h) : t) =
  let r = make (Point.point_reflection pivot p) w h in
  translate (-.w) (-.h) r

let contains ((p, w, h) : t) (pt : Point.t) =
  let open Point in
  p.x < pt.x && pt.x < p.x +. w && p.y < pt.y && pt.y < p.y +. h

let area ((_, w, h) : t) = w *. h

let perimeter ((_, w, h) : t) = 2. *. (w +. h)

let proj_x r =
  let open Point in
  ((bottom_left_corner r).x, (bottom_right_corner r).x)

let proj_y r =
  let open Point in
  ((bottom_left_corner r).y, (top_right_corner r).y)

let intersects (s1 : t) (s2 : t) =
  let a, b = proj_x s1 and c, d = proj_x s2 in
  if a < d && b > c then
    let a, b = proj_y s1 and c, d = proj_y s2 in
    a < d && b > c
  else false

let segments (r : t) =
  let s1 = Segment.make (bottom_right_corner r) (bottom_left_corner r)
  and s2 = Segment.make (bottom_left_corner r) (top_left_corner r)
  and s3 = Segment.make (top_left_corner r) (top_right_corner r)
  and s4 = Segment.make (top_right_corner r) (bottom_right_corner r) in
  [s1; s2; s3; s4]

let is_square ((_, w, h) : t) = w = h

let encompass (p1, w, h) p2 =
  let l = min (Point.x_coord p1) (Point.x_coord p2)
  and t = max (Point.y_coord p1 +. h) (Point.y_coord p2)
  and r = max (Point.x_coord p1 +. w) (Point.x_coord p2)
  and b = min (Point.y_coord p1) (Point.y_coord p2) in
  (Point.make l b, r -. l, t -. b)

let bounding (pts : Point.t list) : t =
  match pts with
  | [x] -> (x, 0., 0.)
  | x :: tl -> List.fold_left encompass (x, 0., 0.) tl
  | [] -> invalid_arg "can't build a bounding rectangle with an empty list"

let intersect_line r l =
  let inter =
    segments r
    |> List.map (fun e -> Segment.intersect_line e l)
    |> List.filter (fun e -> e <> None)
  in
  match inter with [Some a; Some b] -> [a; b] | _ -> []

let centroid r = top_right_corner r |> Point.center (bottom_left_corner r)

let random_point st ((p, w, h) : t) =
  let open Point in
  let x = p.x +. Random.State.float st w
  and y = p.y +. Random.State.float st h in
  make x y

let print fmt ((bl, w, h) : t) =
  Format.fprintf fmt "bottom left corner:%a, width=%f, height:%f" Point.print
    bl w h
