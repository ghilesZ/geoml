module Make (A:Arith.T) = struct

  type t = {
      c00 : A.t; c10 : A.t;
      c01 : A.t; c11 : A.t;
      c02 : A.t; c12 : A.t;
    }

  open A

  let identity = {
      c00 = one; c10 = zero;
      c01 = zero; c11 = one;
      c02 = zero; c12 = zero;
    }

  let translation x y = {
      c00 = one; c10 = zero;
      c01 = zero; c11 = one;
      c02 = x; c12 = y;
    }

  let translate x y m = {
      m with
      c02 = m.c02 +. m.c00 *. x +. m.c01 *. y;
      c12 = m.c12 +. m.c10 *. x +. m.c11 *. y;
    }

  let scaling s = {
      c00 = s; c10 = zero;
      c01 = zero; c11 = s;
      c02 = zero; c12 = zero;
    }

  let scale s m = {
      m with
      c00 = m.c00 *. s;
      c01 = m.c01 *. s;
      c10 = m.c10 *. s;
      c11 = m.c11 *. s;
    }

  let rotation angle =
    let angle = A.to_float angle in
    let c = cos angle |> A.of_float in
    let s = sin angle |> A.of_float in
    {
      c00 = c; c10 = s;
      c01 = A.neg s; c11 = c;
      c02 = zero; c12 = zero;
    }

  let rotate angle m =
    let angle = A.to_float angle in
    let c = cos angle |> A.of_float in
    let s = sin angle |> A.of_float in
    let ms = A.neg s in
    {
      m with
      c00 = m.c00 *. c +. m.c01 *. s;
      c01 = m.c01 *. c +. m.c00 *. ms;
      c10 = m.c10 *. c +. m.c11 *. s;
      c11 = m.c11 *. c +. m.c10 *. ms;
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
end
