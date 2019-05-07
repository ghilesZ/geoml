(* This modules implements verification test over the library *)
open Geoml

let line() =
  let p = Point.orig in
  let exn = Line.Error(Same_coordinates p) in
  Alcotest.check_raises "line of points" exn (fun () -> ignore (Line.of_points p p))

let segment() =
  let exn = Invalid_argument "Segment.equation: parameter must be in [0. ; 1.]" in
  let s = Segment.make Point.orig Point.orig in
  Alcotest.check_raises "segment eq" exn (fun () -> ignore (Segment.equation s 1.2))

let constructors =
  ["Line", `Quick, line]

let operations =
  ["Segment", `Quick, segment]

let () =
  Alcotest.run "test suite"
    ["constructors"     , constructors;
     "operations"       , operations
    ]
