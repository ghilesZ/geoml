open Geom
open Utils

let size_x = 800. 
and size_y = 800.
let padding = 30.
and nb_points = 5000
and title = "K-means computation"
and k = 7

let cur = ref ((Array.make k []),([||]))

let colours = 
  let x = 255 * 255 * 255 in
  let step = x/k in
  let cur = ref 0 in
  let val_to_rgb = fun x ->
    let r = x mod 255
    and g = x / 255
    and b = x / (255*255) in
    Graphics.rgb r g b
  in
  list_make (fun _ ->  let c = val_to_rgb !cur in cur:=!cur+step; c) k
    |> Array.of_list

let push array i x = array.(i) <- x::(array.(i))

let gen_point () =
  gen_point padding (size_x-.padding) padding (size_y-.padding)

let init () =
  let res = Array.make k []
  and bars = Array.make k Point.orig in
  for i = 0 to k-1 do
    let p  = gen_point () in
    bars.(i) <- p;
    push res i p
  done;
  for i = k to nb_points - 1 do
    push res (i mod k) (gen_point ())
  done;
  (res,bars)

let work (x,bars) =
  let get_closest arr x =
    let res = ref (-1) and dist = ref infinity in
    for i = 0 to k-1 do
      let cur = arr.(i) in
      let cur_dist = Point.sq_distance cur x in
      if cur_dist < !dist ||
	(cur_dist = !dist && (Random.float 1.) < 0.5)
      then begin
	res := i;
	dist := cur_dist
      end
    done;
    !res
  in 
  let res =  Array.make k [] in
  Array.iter (fun l ->
    List.iter (fun e -> 
      let i = get_closest bars e in
      push res i e
    ) l
  ) x;
  let bars = Array.map Point.iso_barycenter res in
  res,bars

let frame (k,bars) =
  Drawing.draw_string 25 25 "Press 'r' to compute next step" Graphics.black;
  let i = ref 0 in
  Array.iter (fun l -> 
    List.iter (fun e -> Drawing.draw_point ~lw:3 e colours.(!i)) l;
    if l <> [] then Drawing.draw_polygon ~lw:2 (Polygon.bounding l) Graphics.black;
    incr i)  k;
  Array.iter (fun e -> Drawing.draw_point ~lw:5 e Graphics.red) bars
    
let clear () = Drawing.fill_screen Graphics.white

let handler status =
  let open Graphics in
  if status.key = 'r' then begin
    clear ();
    cur := work (!cur);
    frame !cur
  end
      
let loop state = 
  Graphics.loop_at_exit [Graphics.Key_pressed] handler
    
let doit () =
  Random.self_init ();
  Drawing.open_graph size_x size_y title;
  cur := init();
  frame !cur;
  loop ()

let _ = doit()
