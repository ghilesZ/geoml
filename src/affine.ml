

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

let scaling s = {
  c00 = s; c10 = 0.;
  c01 = 0.; c11 = s;
  c02 = 0.; c12 = 0.;
}

let scale s m = {
  m with
  c00 = m.c00 *. s;
  c10 = m.c10 *. s;
  c01 = m.c01 *. s;
  c11 = m.c11 *. s;
}

let rotation angle = {
  c00 = cos angle; c10 = sin angle;
  c01 = -. sin angle; c11 = cos angle;
  c02 = 0.; c12 = 0.;
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

let apply a b =
  { c00 = b.c00 *. a.c00 +. b.c01 *. a.c10;
    c01 = b.c00 *. a.c01 +. b.c01 *. a.c11;
    c10 = b.c10 *. a.c00 +. b.c11 *. a.c10;
    c11 = b.c10 *. a.c01 +. b.c11 *. a.c11;
    c02 = b.c00 *. a.c02 +. b.c01 *. a.c12 +. b.c02;
    c12 = b.c10 *. a.c02 +. b.c11 *. a.c12 +. b.c12; }

let det m = m.c00 *. m.c11 -. m.c01 *. m.c10

let transform_distance m x y =
  (m.c00 *. x +. m.c01 *. y,  m.c10 *. x +. m.c11 *. y)

let transform_point m x y =
  (m.c00 *. x +. m.c01 *. y +. m.c02,  m.c10 *. x +. m.c11 *. y +. m.c12)
