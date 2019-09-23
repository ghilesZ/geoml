open Math

module Make
         (A:Arith.T)
         (P:Signatures.Point_Sig with type arith = A.t) = struct

  type arith = A.t
  type point = P.t


type t = {f1 : point; f2 : point; radius : arith;
	  big_axis : arith; small_axis : arith}

open A

let make_bifocal p1 p2 radius =
  let p1p2 = P.distance p1 p2 in
  let big_axis = p1p2 +. two *. radius in
  let a = p1p2 /. two in
  let c = ((p1p2 +. radius) /. two) in
  let b = c *. c -. a *. a |> to_float |> sqrt |> of_float in
  { f1=p1; f2=p2; radius
  ; big_axis ; small_axis = b *. two }

let focal1 {f1;_} = f1
let focal2 {f2;_} = f2
let radius {radius;_} = radius
let center {f1;f2;_} = P.center f1 f2
let big_axis {big_axis;_} = big_axis
let small_axis {small_axis;_} = small_axis

let translate dx dy {f1; f2; radius;_} =
  make_bifocal (P.translate dx dy f1) (P.translate dx dy f2) radius

let rotate e c f =
  {e with
    f1=P.rotate e.f1 c f;
    f2=P.rotate e.f2 c f}

let rotate_angle e c f =
  {e with
    f1=P.rotate_angle e.f1 c f;
    f2=P.rotate_angle e.f2 c f}

let abs x = if x > zero then x else neg x

let scale_x {f1;f2;radius;_} f =
  let new_radius =
    if f1.y = f2.y then radius *. f
    else if f1.x = f2.x then radius
    else
      let ratio = (abs (f1.x -. f2.x)) /. (abs (f1.y -. f2.y)) in
      radius *. ratio
  in
  let f1 = P.scale_x f1 f and f2 =P.scale_x f2 f in
  make_bifocal f1 f2 new_radius

let scale_y {f1;f2;radius;_} f =
  let new_radius =
    if f1.x = f2.x then radius *. f
    else if f1.y = f2.y then radius
    else
      let ratio = (abs (f1.y -. f2.y)) /. (abs (f1.x -. f2.x)) in
      radius *. ratio
  in
  let f1 = P.scale_y f1 f and f2 =P.scale_y f2 f in
  make_bifocal f1 f2 new_radius

let contains {f1; f2; radius; _} p =
  let a = P.distance f1 f2 +. radius in
  a >= P.distance f1 p +. P.distance f2 p

let area {big_axis; small_axis; _} =
  (A.of_float pi) *. (big_axis /. two) *. (small_axis /. two)
end
