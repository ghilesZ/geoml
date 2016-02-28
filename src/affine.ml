

type t = {
  c00 : float; c10 : float;
  c01 : float; c11 : float;
  c02 : float; c12 : float;
}

let identity = {
  c00 = 1.; c10 = 0.;
  c01 = 0.; c11 = 1.;
  c02 = 0.; c12 = 0.;
}

let translation x y = {
  c00 = 1.; c10 = 0.;
  c01 = 0.; c11 = 1.;
  c02 = x; c12 = y;
}

let translate x y m = {
  m with
  c02 = m.c02 +. m.c00 *. x +. m.c01 *. y;
  c12 = m.c12 +. m.c10 *. x +. m.c11 *. y;
}

let scale x y = {
  c00 = x; c10 = 0.;
  c01 = 0.; c11 = y;
  c02 = 0.; c12 = 0.;
}

let scale x y m = {
  m with
  c00 = m.c00 *. x;
  c10 = m.c10 *. x;
  c01 = m.c01 *. y;
  c11 = m.c11 *. y;
}


let rotate angle (m : t) =
  let cos = cos angle in
  let sin = sin angle in {
    m with
    c00 = m.c00 *. cos +. m.c01 *. sin;
    c01 = m.c01 *. cos -. m.c00 *. sin;
    c10 = m.c10 *. cos +. m.c11 *. sin;
    c11 = m.c11 *. cos -. m.c10 *. sin;
  }

let det m = m.c00 *. m.c11 -. m.c01 *. m.c10

let transform_distance m x y =
  (m.c00 *. x +. m.c01 *. y,  m.c10 *. x +. m.c11 *. y)

let transform_point m x y =
  (m.c00 *. x +. m.c01 *. y +. m.c02,  m.c10 *. x +. m.c11 *. y +. m.c12)
