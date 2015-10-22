
type t = Point.t list

let make l = l

let fold f acc p =
  let rec aux acc = function
  | [] -> acc
  | [pt] -> f acc pt @@ List.hd p
  | pt1 :: pt2 :: pg ->
    aux (f acc pt1 pt2) (pt2 :: pg)
  in aux acc p

let perimeter p =
  fold Point.(fun acc p1 p2 ->
      acc +. distance p1 p2
    ) 0. p

let area p =
  fold Point.(fun acc p1 p2 ->
      acc +. (p1.x *. p2.y -. p1.y *. p2.x)
    ) 0. p /. 2.

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

  let fold_filter filter f acc rp =
    let rec aux nth acc current next =
      if nth = rp.edges || not @@ filter current next then acc
      else
        aux (nth + 1) (f nth acc current next) next (next_point ~nth:(nth + 1) rp)
    in aux 0 acc rp.fst (next_point rp)

  let perimeter rp = float_of_int rp.edges *. rp.len
  let area rp = rp.len *. rp.apothem /. 2. *. float_of_int rp.edges

  let to_polygon rp =
   fold_filter (fun _ _ -> true)
      (fun _ acc current _ ->
         current :: acc
      ) [] rp

  let to_randomized_polygon rp =
   fold_filter (fun _ _ -> true)
      (fun _ acc current next ->
        if Random.bool () then acc else
        let bar = Point.barycenter
            [(current, Random.float 1.); (next, Random.float 1.)]
        in bar :: acc
      ) [] rp

  let translate rp dx dy =
    { rp with
      center = Point.translate rp.center dx dy;
      fst = Point.translate rp.fst dx dy
    }

  let is_square rp = rp.edges = 4

  let contains rp pt =
    fold_filter
      (fun current next ->
         (0. > Point.determinant current next rp.center)
         = (0. > Point.determinant current next pt)
      ) (fun nth _ current next -> nth = rp.edges) true rp


end
