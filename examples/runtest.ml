let msg = "usage: runtest <testname>"
let spec = []

let file = ref None

let set_file f = match !file with
  | Some _ -> Arg.usage spec msg; exit 1
  | None -> file := Some f

let () = Arg.parse spec set_file msg

let tests =[
   "affine";
   "constraint";
   "inter_poly";
   "regular_polygon";
   "triangulation";
   "bezier";
   "intersection";
   "bounding";
   "ellipse";
   "symetry";
   "welzl_sbs";
   "circum_incircle";
   "fermat";
   "polynom";
   "random";
   "scale";
   "vector";
   "polyhedra"
  ]

let print_tests fmt tests = Format.pp_print_list
  (fun f a -> Format.fprintf f "%s " a)
  fmt
  tests

let () =
  match !file with
  | Some "affine" -> Affine_t.Go.doit ()
  | Some "constraint" -> Constraint_t.Go.doit ()
  | Some "inter_poly" -> Inter_poly_t.Go.doit ()
  | Some "regular_polygon" -> Regular_polygon_t.Go.doit ()
  | Some "triangulation" -> Triangulation_t.Go.doit ()
  | Some "bezier" -> Bezier_t.Go.doit ()
  | Some "intersection" -> Intersetction_t.Go.doit ()
  | Some "bounding" -> Bounding_t.Go.doit ()
  | Some "ellipse" -> Ellipse_t.Go.doit ()
  | Some "symetry" -> Symetry_t.Go.doit ()
  | Some "welzl_sbs" -> Welzl_sbs_t.doit ()
  | Some "circum_incircle" -> Circum_incircle_t.Go.doit ()
  | Some "fermat" -> Fermat_t.Go.doit ()
  | Some "polynom" -> Polynom_t.Go.doit ()
  | Some "random" -> Random_t.Go.doit()
  | Some "scale" -> Scale_t.doit()
  | Some "vector" -> Vector_t.Go.doit()
  | Some "polyhedra" -> Polyhedra_t.Go.doit()
  | Some s -> Format.printf "Test %s not found\n" s
  | None ->
     Format.printf
       "You must specify one of those test name : %a\n" print_tests tests
