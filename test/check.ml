(* This modules implements verification test over the library *)
open Geoml

(*********************************************************************************)

let line_good () =
  let p = Point.orig in
  let p' = Point.make 0. 1. in
  let l = Line.make_x 0. in
  Alcotest.(check bool "Line.of_points" true (Line.of_points p p' = l))

let line_bad () =
  let p0 = Point.orig in
  let exn = Line.Error (Same_coordinates p0) in
  Alcotest.check_raises "Line.of_points" exn (fun () ->
      ignore (Line.of_points p0 p0))

let circle_bad () =
  let p0 = Point.orig in
  let exn =
    Invalid_argument "Circle.make:radius should be positive or zero"
  in
  Alcotest.check_raises "Circle.make" exn (fun () ->
      ignore (Circle.make p0 (-1.)))

(*********************************************************************************)

let l_intersec () =
  let l0 = Line.y_axis and l1 = Line.make_x 3. in
  let exn = Line.Error (Parallel (l0, l1)) in
  Alcotest.check_raises "Line.intersection" exn (fun () ->
      ignore (Line.intersection l0 l1))

(*********************************************************************************)

(* test of the property (contains x (random x)) *)

let random_contains_inside f_gen f_in modulename =
  for _ = 0 to 100000 do
    let p = f_gen () in
    Alcotest.(
      check bool
        ( modulename ^ ".contains shape (" ^ modulename
        ^ ".random_point shape)" )
        true (f_in p))
  done

let rectangle_contains () =
  let st = Random.get_state () in
  let r = Rectangle.make Point.orig max_float max_float in
  let f_gen () = Rectangle.random_point st r in
  random_contains_inside f_gen (Rectangle.contains r) "Rectangle"

let circle_contains () =
  let st = Random.get_state () in
  let r = Circle.make Point.orig max_float in
  let f_gen () = Circle.random_point st r in
  random_contains_inside f_gen (Circle.contains r) "Circle"

let triangle_contains () =
  let st = Random.get_state () in
  let r =
    Triangle.make Point.orig
      (Point.make (-1000000.) 0.)
      (Point.make (-1000000.) 1000000.)
  in
  let f_gen () = Triangle.random_point st r in
  random_contains_inside f_gen (Triangle.contains r) "Triangle"

let constructors =
  [ ("Line", `Quick, line_bad)
  ; ("Line", `Quick, line_good)
  ; ("Circle", `Quick, circle_bad) ]

let operations = [("Line.intersection", `Quick, l_intersec)]

let random_generation =
  [ ("Rectangle.random_point", `Slow, rectangle_contains)
  ; ("Circle.random_point", `Slow, circle_contains)
  ; ("Triangle.random_point", `Slow, triangle_contains) ]

let () =
  Random.self_init () ;
  Alcotest.run "test suite"
    [ ("constructors", constructors)
    ; ("operations", operations)
    ; ("random", random_generation) ]
