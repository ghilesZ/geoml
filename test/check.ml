(* This modules implements verification test over the library *)
open Geoml

let line() =
  let p0 = Point.orig in
  let exn = Line.Error(Same_coordinates p0) in
  Alcotest.check_raises "Line.of_points" exn (fun () -> ignore (Line.of_points p0 p0))

let l_intersec() =
  let l0 = Line.y_axis and l1 = Line.make_x 3. in
  let exn = Line.Error(Parallel(l0,l1)) in
  Alcotest.check_raises "Line.intersection" exn (fun () -> ignore (Line.intersection l0 l1))


let rectangle_contains () =
  let r = Rectangle.make Point.orig max_float max_float in
  for _ = 0 to 1000 do
    let p = Rectangle.random_point r in
    Alcotest.(check bool "Rectangle.random_point is inside rectange" (Rectangle.contains r p) true)
  done

let triangle_contains () =
  let r = Triangle.make Point.orig
            (Point.make max_float max_float)
            (Point.make min_float max_float)
  in
  for _ = 0 to 1000 do
    let p = Triangle.random_point r in
    Alcotest.(check bool "Triangle.random_point is inside triangle" (Triangle.contains r p) true)
  done

let constructors =
  ["Line", `Quick, line]

let operations =
  ["Line.intersection", `Quick, l_intersec]

let random_generation =
  ["Rectangle.random_point", `Slow, rectangle_contains
  ;"Triangle.random_point", `Slow, triangle_contains]

let () =
  Random.self_init();
  Alcotest.run "test suite"
    ["constructors"     , constructors
    ;"operations"       , operations
    ;"random"           , random_generation]
