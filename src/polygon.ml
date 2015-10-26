
type t = Point.t list

let make l = l

let fold f acc p =
  let rec aux acc = function
  | [] -> acc
  | [pt] -> f acc pt @@ List.hd p
  | pt1 :: pt2 :: pg ->
    aux (f acc pt1 pt2) (pt2 :: pg)
  in aux acc p

let fold_filter filter f acc p =
  let rec aux acc = function
  | [] -> acc
  | [pt] ->
    let hd = List.hd p in
    if not @@ filter pt hd then acc
    else f acc pt hd
  | pt1 :: pt2 :: pg ->
    if not @@ filter pt1 pt2 then aux acc (pt2 :: pg)
    else aux (f acc pt1 pt2) (pt2 :: pg)
  in aux acc p

let perimeter p =
  fold Point.(fun acc p1 p2 ->
      acc +. distance p1 p2
    ) 0. p

let area p =
  fold Point.(fun acc p1 p2 ->
      acc +. (p1.x *. p2.y -. p1.y *. p2.x)
    ) 0. p /. 2.

let proj_x p = Point.(fold (
    fun (minx, maxx) current _ ->
      min current.x minx, max current.x maxx
  ) ((List.hd p).x, (List.hd p).x) p)

let proj_y p = Point.(fold (
    fun (miny, maxy) current _ ->
      min current.y miny, max current.y maxy
  ) ((List.hd p).y, (List.hd p).y) p)

let bounding l =
  let bottom_left l =
    let open Point in
    let rec aux best l =
      match l with
      | [] -> best
      | a::b when best.y > a.y || best.y = a.y && a.x < best.x -> aux a b
      |h::tl -> aux best tl
    in aux (List.hd l) (List.tl l) in

  let x_axis = Vector.of_points (Point.orig) (Point.make 1. 0.) in
  let angle pa pb = Vector.angle (Vector.of_points pa pb) x_axis in

  let graham_sort l =
    let p = bottom_left l in
    let comp p1 p2 =
      if p1 = p then 1
      else if p2 = p then -1
      else if angle p p1 > angle p p2 then 1 
      else if angle p p1 = angle p p2 then 0
      else -1
    in
    List.sort comp l in

  let signProd a b c =
    let open Point in
    (b.x-.a.x)*.(c.y-.a.y)-.(b.y-.a.y)*.(c.x-.a.x)
  in

  let graham cloud =
    let rec graham_aux cl conv =
      match cl,conv with
      | ([],_) -> conv
      | (h::t, a::b::tl) -> 
      let p = signProd b a h in
      if p <= 0. then graham_aux cl (b::tl)
      else graham_aux t (h::conv)
      | (h::t,_) -> graham_aux t (h::conv)
    in graham_aux (graham_sort cloud) []
  in 
  match l with
  | [] -> failwith "can't build convex envelop with no points"
  | _ -> graham l

let minmax_xy p = Point.(fold (fun (minx, miny, maxx, maxy) current _ ->
    min current.x minx, min current.y miny, max current.x maxx, max current.y maxy
  ) ((List.hd p).y, (List.hd p).y, (List.hd p).y, (List.hd p).y) p)

let contains p pt =
  fold_filter
    Point.(fun i j ->
        ((i.y < pt.y) <> (j.y > pt.y)) &&
        (pt.x < (j.x -. i.x) *. (pt.y -. i.y) /. (j.y -. i .y) +. i.x))
    (fun acc _ _ -> not acc) false p

let intersection p1 p2 =
  let minx1, miny1, maxx1, maxy1 = minmax_xy p1 in
  let minx2, miny2, maxx2, maxy2 = minmax_xy p2 in
  if maxx1 < minx2 || maxy1 < miny2
     || maxx2 < minx1 || maxy2 < miny2 then
    []
  else
    let _, (_p1_inter, inters, _entering) =
      fold (fun (inside, (p1_inter, inters, entering)) cur1 next1 ->

          let _, (p1_inter, inters, entering) =
            fold (fun (inside, (p1_inter, inters, entering)) cur2 next2 ->
                match
                  Segment.(intersection (make cur1 next1) (make cur2 next2))
                with
                | None -> inside, (p1_inter, inters, entering)
                | Some v ->
                  not inside,
                  (v :: p1_inter, v :: inters,
                   if inside then entering else v :: entering)
                | exception (Line.Error e) ->
                  Format.printf "%a\n%a\n%a\n%a\n"
                    Point.print cur1 Point.print next1
                    Point.print cur2 Point.print next2;
                  Format.printf "%a@." Line.print_error e;
                  assert false
              ) (inside, (p1_inter, inters, entering)) p2
          in
          inside, (p1_inter, inters, entering)

      ) (contains p1 (List.hd p2), ([],[],[])) p1
    in
    inters


module Regular = struct

  type t = {
    center : Point.t;
    fst : Point.t; (* one arbitrary point of the regular polygon *)
    snd : Point.t;
    len : float;
    apothem : float;
    edges : int;
  }

  let make center fst edges =
    let snd = Point.rotate_angle center fst (360. /. float_of_int edges) in
    { center; fst; snd; edges;
      len = Point.distance fst snd;
      apothem =
        Point.distance center (Point.iso_barycenter [fst; snd])
    }

  let next_point ?(nth=0) rp =
    if nth = rp.edges - 1 then rp.fst
    else if nth = 0 then rp.snd
    else
      Point.rotate_angle rp.center rp.fst
        (float_of_int (nth + 1) *. (360. /. float_of_int rp.edges))

  let fold_stop filter f acc rp =
    let rec aux nth acc current next =
      if nth = rp.edges || not @@ filter current next then acc
      else
        aux (nth + 1) (f nth acc current next) next (next_point ~nth:(nth + 1) rp)
    in aux 0 acc rp.fst (next_point rp)

  let perimeter rp = float_of_int rp.edges *. rp.len
  let area rp = rp.len *. rp.apothem /. 2. *. float_of_int rp.edges

  let to_polygon rp =
   fold_stop (fun _ _ -> true)
      (fun _ acc current _ ->
         current :: acc
      ) [] rp

  let to_randomized_polygon ?(min=3)  ?(prob=0.5) rp =
   fold_stop (fun _ _ -> true)
      (fun _ (prob, acc) current next ->
        if Random.float 1. < prob then acc else
        let rand = Random.float 1. in
        let bar = Point.barycenter [(current, rand); (next, 1. -. rand)] in
        (prob + prob / min, bar :: acc)
      ) (prob, []) rp

  let translate rp dx dy =
    { rp with
      center = Point.translate rp.center dx dy;
      fst = Point.translate rp.fst dx dy
    }

  let is_square rp = rp.edges = 4

  let contains rp pt =
    fold_stop
      (fun current next ->
         (0. > Point.determinant current next rp.center)
         = (0. > Point.determinant current next pt)
      ) (fun nth _ current next -> nth = rp.edges) true rp


end
