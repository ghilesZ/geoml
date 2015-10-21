
type t = Point.t list

let make l = l

let perimeter =
  let rec aux acc = function
  | [] -> acc
  | [pt] -> acc
  | pt1 :: pt2 :: pg ->
    aux (acc +. Point.distance pt1 pt2) (pt2 :: pg)
  in aux 0.

let area p = assert false

module Regular = struct

  type t = {
    center : Point.t;
    fst : Point.t; (* one arbitrary point of the regular polygon *)
    len : float;
    apothem : float;
    edges : int;
  }

  let make center fst len edges = {
    center; fst; len; edges;
    apothem =
      let snd = Point.rotate center fst (360. /. float_of_int edges) in
      Point.distance center (Point.barycenter [fst; snd])
  }

  let perimeter rp = float_of_int rp.edges *. rp.len
  let area rp = rp.len *. rp.apothem /. 2. *. float_of_int rp.edges

  let translate rp dx dy =
    { rp with
      center = Point.translate rp.center dx dy;
      fst = Point.translate rp.fst dx dy
    }

  let is_square rp = rp.edges = 4

  let next_point ?(nth=0) rp pt =
    if nth = rp.edges then rp.center
    else
      Point.rotate rp.center rp.fst (360. /. float_of_int rp.edges)

  let fold_filter filter f acc rp =
    let rec aux nth acc current next =
      if nth = rp.edges then acc
      else if not @@ filter current next then acc
      else
        let nth = nth + 1 in
        aux (nth + 1) (f nth current next) next (next_point ~nth rp next)
    in aux 0 acc rp.fst (next_point rp rp.fst)

  let contains rp pt =
    fold_filter
      (fun current next ->
         (0. > Point.determinant current next rp.center)
         = (0. > Point.determinant current next pt)
      ) (fun nth current next -> nth = rp.edges) true rp


end
