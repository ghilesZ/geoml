open Geom
open Utils

let size_x = 1000. 
and size_y = 1000.
let padding = 30.
and nb_points = 100000
and title = "K-means computation"
let nb_iter = 2
let k = 10000

let kmean k ptl =
  let cpt = ref 0 in
  let push array i x = array.(i) <- x::(array.(i)) in
  let init () =
    let res = Array.make k []
    and bars = Array.make k Point.orig in
    let rec aux l i =
      if i < k then begin
	let p  = List.hd l in
	bars.(i) <- p;
	push res i p;
	aux (List.tl l) (i+1)
      end
      else l
    in
    let rest = aux ptl 0 in
    let rec aux i = function
      | [] -> ()
      | hd::tl -> push res (i mod k) hd;
	aux (i+1) tl
    in
    aux k rest;
    (res,bars)
  in
  let work (x,bars) =
    let get_closest arr x =
      let res = ref (-1) and dist = ref infinity in
      for i = 0 to k-1 do
	let cur = arr.(i) in
	let cur_dist = Point.sq_distance cur x in
	if cur_dist < !dist || cur_dist = !dist then begin
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
  in
  let (res,bars) = init () in
  let rec doit (res,bars) =
    print_int (!cpt);
    print_newline();
    let (res_next,bars_next) = work (res,bars) in
    if !cpt=nb_iter then res
    else begin
      incr cpt;
      doit (res_next,bars_next)
    end
  in doit (res,bars)

let parse s = 
  let open Scanf in 
  let ic = open_in s in
  let arr = Array.make 100000 Point.orig in
  try
    fscanf ic "%s\n" (fun x -> x) |> ignore;
    while true do
      fscanf ic "%d,%f,%f,%f\n"
	(fun id x y _ -> arr.(id - 1) <- Point.make ((y+.220.)*.2.) ((x+.110.)*.5.) )
    done;
    assert false
  with
  | End_of_file -> arr

let frame k =
  Drawing.draw_string 25 25 "Press 'r' to compute next step" Graphics.black;
  let i = ref 0 in
  Array.iter (fun l -> 
    List.iter (fun e -> Drawing.draw_point ~lw:2 e Graphics.red) l;
    if l <> [] then Drawing.draw_polygon ~lw:2 (Polygon.bounding l) Graphics.black;
    incr i) k
    
let clear () = Drawing.fill_screen Graphics.white

let handler status =
  let world = parse "world.csv" |> Array.to_list in
  clear ();
  world |> kmean k |> frame
      
let loop state = 
  Graphics.loop_at_exit [Graphics.Key_pressed] handler
    
let doit () =
  Random.self_init ();
  Drawing.open_graph size_x size_y title;
  loop ()
