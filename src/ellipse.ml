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
let big_axis {big_axis;_} = big_axis
let small_axis {small_axis;_} = small_axis

let translate {f1; f2; radius;_} dx dy = 
  make_bifocal (Point.translate f1 dx dy) (Point.translate f2 dx dy) radius

let contains {f1; f2; radius; _} p =
  let a = Point.distance f1 f2 +. radius in
  a >= Point.distance f1 p +. Point.distance f2 p

let area {big_axis; small_axis; _} =
  pi *. (big_axis /. 2.) *. (small_axis /. 2.)
