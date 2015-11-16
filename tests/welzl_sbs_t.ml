open Geom
open Circle

let size_x = 600. 
and size_y = 400.
let padding = 100.
and size = 10
and title = "Calculating the bounding circle of a point list"
let cur = ref ((make Point.orig 0.),[],[],[])

let of_two x y pt =
  try 
    [((x,pt),y);((y,pt),x)]
    |> List.map (fun ((a,b),c) -> (([a;b],of_diameter a b),c))
    |> List.find (fun ((env,circle),inner) -> contains circle inner)
    |> fst
  with Not_found -> ([x;y;pt],(circumscribed x y pt))

let of_three x y z pt =
  let found =
    [((x,y),z); ((x,z),y); ((y,z),x)]
    |> List.map (fun ((a,b),c) -> ((of_two a b pt),c))
    |> List.filter (fun ((e,c),inner) -> contains c inner)
    |> List.map fst
  in
  List.fold_left (fun (e1,c1) (e2,c2) ->
    if radius c1 < radius c2 then (e1,c1) else (e2,c2)
  ) (List.hd found) (List.tl found)

let update set pt =
  match set with
  | [x] -> [x;pt],(of_diameter x pt)
  | [x;y] -> of_two x y pt
  | [x;y;z] -> of_three x y z pt
  | _ -> assert false  

let mindisk (circle,set,computed,pts) =
  match pts with
  | [] -> (circle,set,computed,pts)
  | h::tl when contains circle h -> (circle,set,(h::computed),tl)
  | h::tl -> 
     let (new_set,new_circle) = update set h in
     let new_bidule =
       List.filter (fun e -> List.mem e new_set |> not) (computed@pts)
     in 
     (new_circle,new_set,[],new_bidule)

let clear () = Drawing.fill_screen Graphics.white

let init () =
  let pts = Utils.list_make
    (fun _ -> Utils.gen_point padding (size_x-.padding) padding (size_y-.padding))
    size
  and computed = [] in
  let h = List.hd pts in
  let enveloppe = [h]
  and circle = make h 0.
  in (circle,enveloppe,computed,(List.tl pts))

let frame (circle,enveloppe,computed,other) =
  Drawing.draw_string 25 25 "Press 'r' to compute next step" Graphics.black;
  Drawing.draw_circle circle Graphics.red;
  (try
    let c = Circle.make (List.hd other) 7. in
    Drawing.fill_circle c (Graphics.rgb 255 120 50)
  with _ -> ());
  List.iter (fun e ->
      let c = Circle.make e 5. in 
      Drawing.fill_circle c Graphics.black
  ) other;
  List.iter (fun e -> 
    let c = Circle.make e 5. in 
    Drawing.fill_circle c (Graphics.rgb 180 180 180)
  ) computed;
  List.iter (fun e -> 
    let c = Circle.make e 5. in 
    Drawing.fill_circle c (Graphics.rgb 255 79 0)
  ) enveloppe


let save_ppm nb = 
  let img = Drawing.get_image size_x size_y in
  Drawing.to_ppm img ("img/img"^(string_of_int nb))

let nb = ref 0

let handler status =
  let open Graphics in
  if status.key = 'r' then begin
    save_ppm !nb;
    clear ();
    cur := mindisk (!cur);
    incr nb;
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
