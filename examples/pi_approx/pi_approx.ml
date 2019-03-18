open Geoml

let square = Rectangle.make Point.orig 1. 1.

let circle = Circle.make Point.orig 1.

let approx nb_iter =
  let rec loop inside = function
    | 0 -> inside
    | n ->
       let pt = Rectangle.random_point square in
       loop
         (if Circle.contains circle pt then inside + 1
          else inside)
         (n-1)
  in
  let inside = loop 0 nb_iter in
  4. *. (float inside) /. (float nb_iter)

let main =
  Random.self_init();
  let nb = Sys.argv.(1) |> int_of_string in
  Format.printf "pi approximation after %i iterations:\n%.30f\n%!" nb (approx nb)
