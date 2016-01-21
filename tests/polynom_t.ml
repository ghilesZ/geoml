open Geom
open Utils

module Pol = struct
  let size_x = 800.
  and size_y = 700.
  and padding = 50.
  and title = "Polynoms tests"

  type t = Polynom.t list
    
  let new_val () = 
    let p = Polynom.make [8.;2.;200.] in
    let p' = Polynom.derive p in 
    [p;p']

  let frame pol_list = 
    List.iter (fun e -> Drawing.draw_polynom e Graphics.blue) pol_list

end

module Go = Tester.Make(Pol)

let _ =  Go.doit()
