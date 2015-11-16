
type t = Point.t list

type polygon = t

let make l = l
let to_list l = l
let first_point = List.hd

let fold f acc p =
  let rec aux acc = function
  | [] -> acc
  | [pt] -> f acc pt @@ List.hd p
  | pt1 :: pt2 :: pg ->
    aux (f acc pt1 pt2) (pt2 :: pg)
  in aux acc p

let map = List.rev_map

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

let fold_segments_pair f acc p =
  let rec aux acc p' =
    match p' with
    | [] | [_] -> acc
    | [v; v1] ->
      let hd = List.hd p in
      f (f acc v v1 hd)
        v1 hd List.(hd @@ tl p)
    | v1 :: v2 :: v3 :: p' ->
      aux (f acc v1 v2 v3) (v2 :: v3 :: p')
  in
  aux acc p

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
        (((i.y <= pt.y) && (pt.y < j.y)) ||
         ((j.y <= pt.y) && (pt.y < i.y))) &&
        (pt.x < (j.x -. i.x) *. (pt.y -. i.y) /. (j.y -. i.y) +. i.x))
    (fun acc _ _ -> not acc) false p


let segments_intersection_points crossing p1 p2 =
  let minx1, miny1, maxx1, maxy1 = minmax_xy p1 in
  let minx2, miny2, maxx2, maxy2 = minmax_xy p2 in
  let update h default f cpl newitem =
    let res =
      try Segment.Tbl.find h cpl with Not_found -> default
    in Segment.Tbl.add h cpl (f newitem res)
  in
  if maxx1 < minx2 || maxy1 < miny2
     || maxx2 < minx1 || maxy2 < miny2 then
    []
  else
    let update seg1 v seg2 =
      let u seg = update crossing [] (fun e ol -> e :: ol) seg v in
      u seg1; u seg2
    in
    fold (fun inters cur1 next1 ->
        fold (fun inters cur2 next2 ->
            match Segment.(intersection (make cur1 next1) (make cur2 next2))
            with None -> inters | Some v ->
              update (cur2, next2) v (cur1, next1);
              v :: inters
          ) inters p2) [] p1


(* Weiler Atherton #####################################################  *)

let insert_intersection_points crossing crosslink entering start_inside l = snd @@ fold (
    fun (inside, acc) cur next ->
      try
        let vs = Segment.Tbl.find crossing (cur, next) in
        let vs = Common.List.split_concat_sorted
            (fun v1 v2 -> Pervasives.compare
                (Point.sq_distance cur v1)
                (Point.sq_distance cur v2)) vs
        in
        List.fold_left (fun (inside, acc) cross ->
            if not inside then begin
              entering := cross :: !entering;
            end else Point.Tbl.add crosslink cross acc ;
            not inside, (cross, (Some (inside))) :: acc
          ) (inside, ((cur, None) :: acc)) vs
      with Not_found -> inside, (cur, None) :: acc
  ) (start_inside, []) l

let rec build_clip p1 p2 htbl acc l =
  let rec aux_build_clip htbl start head head' enterings acc l =
    match l with
    | [] -> aux_build_clip htbl start head head' enterings acc head
    | (vert, carac) :: l' ->
      if start == vert then enterings, acc
      else begin match carac with
        | None -> aux_build_clip htbl start head head' enterings (vert :: acc) l'
        | Some false ->
          let enterings' = match enterings with
            | [] -> enterings
            | h :: t -> if h == vert then t else enterings
          in
          let link = Point.Tbl.find htbl vert in
          aux_build_clip htbl start head' head enterings' (vert :: acc) link
        | Some true -> Format.printf "FAIL@."; assert false
      end
  in
  match l with
  | [] -> acc
  | ent :: l' ->
    try
      let link = Point.Tbl.find htbl ent in
      let l', poly = aux_build_clip htbl ent p1 p2 l' [ent] link in
      build_clip p1 p2 htbl (poly :: acc) l'
    with Not_found -> Format.printf "FAIL@."; assert false

let intersection_polygons p1 p2 =
  match p1, p2 with | [], _ | _, [] -> [] | _ ->
    let start_inside_p1 = contains p1 (List.hd p2) in
    let start_inside_p2 = contains p2 (List.hd p1) in
    let crossing = Segment.Tbl.create 19 in
    let crosslink = Point.Tbl.create 11 in
    let inters = segments_intersection_points crossing p1 p2 in
    if inters == [] then
      if start_inside_p1 then [p2]
      else [p1]
    else
      let enterings_p1 = ref [] in
      let enterings_p2 = ref [] in
      let newp1 =
        insert_intersection_points crossing crosslink
          enterings_p1 (start_inside_p2) p1
      in
      let newp2 =
        insert_intersection_points crossing crosslink
          enterings_p2 (start_inside_p1) p2
      in begin match p1, p2 with
        | [a; b], _ | _, [a; b] -> [inters]
        | _ -> build_clip newp1 newp2 crosslink [] !enterings_p2
      end
(* ################################################################## *)


(* Polygon triangulation ############################################ *)
module AngleSet = Set.Make (struct
    open Point
    type t = Point.t * float
    let compare (pt1, angle1) (pt2, angle2) =
      if pt1 = pt2 then 0
      else if angle1 <= angle2 then -1 else 1
  end)

let compute_ears h p =
  fold_segments_pair (fun (n, acc) v1 v2 v3 ->
      let vec1 = Vector.of_points v2 v1 in
      let vec2 = Vector.of_points v2 v3 in
      let angle = Vector.angle_deg vec1 vec2 in
      Hashtbl.add h v2 (v1, angle, v3);
      if angle > 180. then (n + 1, acc)
      else n + 1, AngleSet.add (v2, angle) acc
    ) (0, AngleSet.empty) p


let print_pt_name htbl fmt pt =
  try Format.fprintf fmt "%s" @@ Hashtbl.find htbl pt with
  | Not_found -> Format.fprintf fmt "%a" Point.print pt


let create_names p =
  let t = Hashtbl.create 19 in
  let cpt = ref 64 in
  List.iter (
    fun pt -> incr cpt;
      Hashtbl.add t pt (Format.sprintf "%c" (Char.chr !cpt))) p; t


let triangulation p =
  let htbl = Hashtbl.create 19 in
  let ptnames = create_names p in
  let printpt = print_pt_name ptnames in
  let n, ears = compute_ears htbl p in
  let rec aux acc nb ears =
    if AngleSet.is_empty ears then acc else
      try
        let (v2, angle) = AngleSet.min_elt ears in
        let (v1, _, v3) = Hashtbl.find htbl v2 in
        let tr = [v1; v2; v3] in
        let acc' = (v1, v2, v3) :: acc in

        Format.printf "ears (%a) :" printpt v2;

        Format.printf "(%a)  ->   \n"
          (Common.List.print_sep printpt ", ") tr;

        let (v1_1, v1_angle, _) = Hashtbl.find htbl v1 in
        let (_, v3_angle, v3_3) = Hashtbl.find htbl v3 in

        let new_angle_v1 = Vector.(angle_deg (of_points v1 v1_1) (of_points v1 v3)) in
        let new_angle_v3 = Vector.(angle_deg (of_points v3 v1) (of_points v3 v3_3)) in

        Format.printf "    [%a]@."
          (Common.List.print_sep (fun fmt (pt, angle) ->
               Format.fprintf fmt "%a" (print_pt_name ptnames) pt) "; ")
        @@ AngleSet.elements ears;

        let v1c = (v1, v1_angle) in
        let v3c = (v3, v3_angle) in


        let ears' = AngleSet.(

          ears
          |> AngleSet.remove (v2, angle)
          |> (fun s -> if mem v1c s then s |> remove v1c |> add (v1, new_angle_v1) else s)
          |> (fun s -> if mem v3c s then s |> remove v3c |> add (v3, new_angle_v3) else s)
          )
        in

        Format.printf "    [%a]@."
          (Common.List.print_sep (fun fmt (pt, angle) ->
               Format.fprintf fmt "%a" (print_pt_name ptnames) pt) "; ")
        @@ AngleSet.elements ears';


        Hashtbl.add htbl v1 (v1_1, new_angle_v1, v3);
        Hashtbl.add htbl v3 (v3_3, new_angle_v3, v1);
        aux acc' (succ nb) ears'
      with
      | Not_found -> Format.printf "not found@."; acc
  in
  let res = aux [] 0 ears in
  res, List.map (fun pt -> pt, Hashtbl.find ptnames pt) p



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

  let to_randomized_polygon ?(minp=3)  ?(prob=0.5) rp =
   snd @@ fold_stop (fun _ _ -> true)
      (fun _ (nb, acc) current next ->
        let minp = float_of_int minp in
        let prob =
          if nb >= minp then prob
          else max prob (minp /. (float_of_int rp.edges -. nb))
        in
        if Random.float 1. > prob then (nb, acc)
        else
          let rand = Random.float 1. in
          let bar = Point.barycenter [(current, rand); (next, 1. -. rand)] in
          (nb +. 1., bar :: acc)
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

  let map f rp =
    { rp with
      center = f rp.center;
      fst = f rp.fst;
      snd = f rp.snd;
    }


end
