open Geom
open Utils

module Half = struct
  let size_x = 800.
  and size_y = 600.
  and title = "Half Space"
    
  let padding = 40.

  type t = Constraint.t * Constraint.t * Point.t list

  let gen () =
    let l = gen_line padding (size_x-.padding) padding (size_y-.padding)
    and comp = Constraint.Gt in
    Constraint.make l comp

  let new_val () =
    ((gen ()),(gen ()),
    (list_make (fun _ -> gen_point padding (size_x-.padding) padding (size_y-.padding)) 50000))

  let frame (c1,c2,lp) =
    Drawing.draw_string 25 585 "Press 'R' refresh" Graphics.black;
    Drawing.draw_line (Constraint.get_border c1) Graphics.red;
    Drawing.draw_line (Constraint.get_border c2) Graphics.blue;
    List.iter 
      (fun p -> Drawing.draw_point p 
	(if Constraint.contains c1 p && Constraint.contains c2 p 
	 then Graphics.magenta
 	 else if Constraint.contains c1 p then Graphics.red
	 else if Constraint.contains c2 p then Graphics.blue
	 else Graphics.green)
      ) lp
    
end
module Go = Tester.Make(Half)
let _ =  Go.doit()
