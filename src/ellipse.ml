open Math 

type t = {f1 : Point.t; f2 : Point.t; radius : float; 
	  big_axis : float; small_axis : float}

let make_bifocal p1 p2 radius = 
  let p1p2 = Point.distance p1 p2 in
  let big_axis = p1p2 +. 2. *. radius in
  let a = p1p2 /. 2. in
  let c = ((p1p2 +. radius) /. 2.) in
  let b = sqrt (c *. c -. a *. a) in
  { f1=p1; f2=p2; radius
  ; big_axis ; small_axis = b *. 2. }

let focal1 {f1;_} = f1
let focal2 {f2;_} = f2
let radius {radius;_} = radius
let center {f1;f2;_} = Point.center f1 f2
let big_axis {big_axis;_} = big_axis
let small_axis {small_axis;_} = small_axis

let translate {f1; f2; radius;_} dx dy = 
  make_bifocal (Point.translate f1 dx dy) (Point.translate f2 dx dy) radius

let rotate e c f =
  {e with 
    f1=Point.rotate e.f1 c f;
    f2=Point.rotate e.f2 c f}

let rotate_angle e c f =
  {e with 
    f1=Point.rotate_angle e.f1 c f;
    f2=Point.rotate_angle e.f2 c f}

let scale_x {f1;f2;radius;_} f =
  let open Point in
  let new_radius = 
    if f1.y = f2.y then radius *. f
    else if f1.x = f2.x then radius
    else 
      let ratio = (abs_float (f1.x -. f2.x)) /. (abs_float (f1.y -. f2.y)) in
      radius *. ratio
  in
  let f1 = Point.scale_x f1 f and f2 =Point.scale_x f2 f in
  make_bifocal f1 f2 new_radius

let scale_y {f1;f2;radius;_} f =
  let open Point in
  let new_radius = 
    if f1.x = f2.x then radius *. f
    else if f1.y = f2.y then radius
    else 
      let ratio = (abs_float (f1.y -. f2.y)) /. (abs_float (f1.x -. f2.x)) in
      radius *. ratio
  in
  let f1 = Point.scale_y f1 f and f2 =Point.scale_y f2 f in
  make_bifocal f1 f2 new_radius

let contains {f1; f2; radius; _} p =
  let a = Point.distance f1 f2 +. radius in
  a >= Point.distance f1 p +. Point.distance f2 p

let area {big_axis; small_axis; _} =
  pi *. (big_axis /. 2.) *. (small_axis /. 2.)
