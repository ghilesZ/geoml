

open Geom
open Utils

module Poly_r = struct
  let size_x = 800.
  and size_y = 700.
  and title = "Random regular polygon"

  type t = Polygon.t * Polygon.t * Polygon.t

  let (!%) f = fun a b -> f b a

  let new_val () =
    let open Polygon in
    (* let rp = (gen_regular 200. (size_x-.200.) 200. (size_y-.200.)) in *)
    let rp = Regular.make (Point.make 200. 200.) (Point.make 300. 300.) 4 in
    let p0 = Regular.to_polygon rp in
    let center = rp.Regular.center in
    Drawing.draw_point center Graphics.red;
    let p1 = Polygon.transform Affine.(
        translate 200. 200.
        @@ scale 0.5
        (* @@ translate ~-.200. ~-.200. *)
        @@ identity
      ) p0
    in
    let p2 = Polygon.transform Affine.(
        translate 200. 200.
        (* @@ translate ~-.200. ~-.200. *)
        @@ identity
      ) p0
    in
    let p3 = Polygon.transform Affine.(
        scale 0.5
        (* @@ translate ~-.200. ~-.200. *)
        @@ identity
      ) p0
    in
    p1, p2, p3

  let frame (p1, p2, p3) =
    Drawing.draw_string 25 675 "few test on affine transformations" Graphics.black;
    Graphics.set_line_width 1;
    Drawing.draw_polygon p1 Graphics.red;
    Drawing.draw_polygon p2 Graphics.blue;
    Drawing.draw_polygon p3 Graphics.black


end
module Go = Tester.Make(Poly_r)
