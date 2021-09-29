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

let bottom_left ((p, _, _) : t) = p

let bottom_right ((p, w, _) : t) = Point.translate w 0. p

let top_right ((p, w, h) : t) = Point.translate w h p

let top_left ((p, _, h) : t) = Point.translate 0. h p

let scale_x ((p, w, h) : t) f = (Point.scale_x p f, w *. f, h)

let scale_y ((p, w, h) : t) f = (Point.scale_y p f, w, h *. f)

let scale r f = scale_y (scale_x r f) f

let translate dx dy ((p, w, h) : t) : t = (Point.translate dx dy p, w, h)

let reflection pivot ((p, w, h) : t) =
  let r = make (Point.reflection pivot p) w h in
  translate (-.w) (-.h) r

let contains ((p, w, h) : t) (pt : Point.t) =
  let open Point in
  p.x < pt.x && pt.x < p.x +. w && p.y < pt.y && pt.y < p.y +. h

let area ((_, w, h) : t) = w *. h

let perimeter ((_, w, h) : t) = 2. *. (w +. h)

let proj_x r = Point.((bottom_left r).x, (bottom_right r).x)

let proj_y r = Point.((bottom_left r).y, (top_right r).y)

let intersects (s1 : t) (s2 : t) =
  let a, b = proj_x s1 and c, d = proj_x s2 in
  a < d && b > c
  &&
  let a, b = proj_y s1 and c, d = proj_y s2 in
  a < d && b > c

let segments (r : t) =
  let s1 = Segment.make (bottom_right r) (bottom_left r)
  and s2 = Segment.make (bottom_left r) (top_left r)
  and s3 = Segment.make (top_left r) (top_right r)
  and s4 = Segment.make (top_right r) (bottom_right r) in
  [s1; s2; s3; s4]

let is_square ((_, w, h) : t) = w = h

let encompass (p1, w, h) p2 =
  Point.(
    let l = min p1.x p2.x
    and t = max (p1.y +. h) p2.y
    and r = max (p1.x +. w) p2.x
    and b = min p1.y p2.y in
    (make l b, r -. l, t -. b))

let bounding (pts : Point.t list) : t =
  match pts with
  | [x] -> (x, 0., 0.)
  | x :: tl -> List.fold_left encompass (x, 0., 0.) tl
  | [] -> invalid_arg "bounding rectangle with an empty list"

let intersect_line r l =
  segments r |> List.filter_map (fun e -> Segment.intersect_line e l)

let centroid r = top_right r |> Point.center (bottom_left r)

let random_point st ((p, w, h) : t) =
  let open Point in
  let x = p.x +. Random.State.float st w
  and y = p.y +. Random.State.float st h in
  make x y

let print fmt ((bl, w, h) : t) =
  Format.fprintf fmt "bottom left corner:%a, width=%f, height:%f" Point.print
    bl w h
