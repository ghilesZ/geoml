let msg = "usage: runtest <testname>"
let spec = []

let file = ref None
let set_file f = match !file with
  | Some _ -> Arg.usage spec msg; exit 1
  | None -> file := Some f

let () = Arg.parse spec set_file msg

let () =
  match !file with
  | Some "affine_t" -> Affine_t.Go.doit ()
  | Some "constraint_t" -> Constraint_t.Go.doit ()
  | Some "inter_poly_t" -> Inter_poly_t.Go.doit ()
  | Some "regular_polygon_t" -> Regular_polygon_t.Go.doit ()
  | Some "triangulation_t" -> Triangulation_t.Go.doit ()
  | Some "bezier_t" -> Bezier_t.Go.doit ()
  | Some "intersetction_t" -> Intersetction_t.Go.doit ()
  | Some "bounding_t" -> Bounding_t.Go.doit ()
  | Some "ellipse_t" -> Ellipse_t.Go.doit ()
  | Some "symetry_t" -> Symetry_t.Go.doit ()
  | Some "welzl_sbs_t" -> Welzl_sbs_t.doit ()
  | Some "circum_incircle_t" -> Circum_incircle_t.Go.doit ()
  | Some "fermat_t" -> Fermat_t.Go.doit ()
  | Some "polynom_t" -> Polynom_t.Go.doit ()
  | Some "random_t" -> Random_t.Go.doit()
  | _ -> ()
