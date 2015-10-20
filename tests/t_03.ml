module T = struct
  let size_x = 800. 
  and size_y = 700. 
  and size = 10
  and title = "Calculating the bounding circle of a point list using emo welzl algorithm. Computation is done in linear time"
    
(************* RANDOM GENERATION ****************)
  let gen_point xmin xmax ymin ymax =
    let x = xmin +. Random.float (xmax-.xmin)
    and y = ymin +. Random.float (ymax-.ymin) in
    Point.make x y
      
  let cur = 
    Random.self_init ();
    let l = Array.make size 0 |> Array.to_list|>
	List.map (fun _ ->
	  gen_point 70. (size_x-.70.) 70. (size_y-.70.)
	) 
    in ref l
    
  let new_list () = 
    cur := List.map (fun _ ->
    gen_point 70. (size_x-.70.) 70. (size_y-.70.)
    ) (!cur)
      
  let frame () =
    new_list ();
    Drawing.draw_string 25 675 "Press 'r' to generate a new cloud" Graphics.black;
    List.iter (fun e -> 
      let c = Circle.make e 5. in 
      Drawing.fill_circle c Graphics.blue
    ) !cur;
    Drawing.draw_circle (Circle.bounding !cur) Graphics.red
end

module Go = Tester.Make(T)

let _ = Go.doit()      
