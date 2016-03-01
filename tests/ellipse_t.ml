open Geom
open Utils

module Ellipse_r = struct
  let size_x = 800.
  and size_y = 600.
  and title = "ellipse"
    
  let padding = 20.

  type t = Ellipse.t * Point.t list

  let gen_ellipse () =
    let f1 : Point.t = gen_point padding (size_x-.padding) padding (size_y-.padding)
    and f2 : Point.t  = gen_point padding (size_x-.padding) padding (size_y-.padding) 
    and r =  Random.float 50. +. 5. in
    Ellipse.make_bifocal f1 f2 r      

  let new_val () =
    (gen_ellipse ()),
    list_make (fun _ -> gen_point padding (size_x-.padding) padding (size_y-.padding)) 40000

  let frame (e,lp) =
    Drawing.draw_string 25 585 "Press 'R' refresh" Graphics.black;
    let e' = Ellipse.translate (Random.float 300.) (Random.float 300.) e in
    List.iter 
      (fun p -> Drawing.draw_point p 
	(if Ellipse.contains e p && Ellipse.contains e' p 
	 then Graphics.magenta
 	 else if Ellipse.contains e p then Graphics.red
	 else if Ellipse.contains e' p then Graphics.blue
	 else Graphics.green)
      ) lp
    
end
module Go = Tester.Make(Ellipse_r)
