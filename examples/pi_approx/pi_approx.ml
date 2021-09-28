open Geoml

let s = Rectangle.make Point.orig 1. 1.

let c = Circle.make Point.orig 1.

let approx nb_iter =
  let st = Random.get_state () in
  let rec loop ok = function
    | 0 -> ok
    | n ->
        loop
          ( if Circle.contains c (Rectangle.random_point st s) then ok + 1
          else ok )
          (n - 1)
  in
  let inside = loop 0 nb_iter in
  4. *. (float inside /. float nb_iter)

let main =
  if Array.length Sys.argv > 1 then
    let nb = Sys.argv.(1) |> int_of_string in
    Format.printf "pi approximation after %i iterations:\n%f\n%!" nb
      (approx nb)
  else Format.printf "please specify a number (int) of iterations\n"
