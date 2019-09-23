module Make
         (A:Arith.T)
         (Aff:Signatures.Affine_Sig with type arith = A.t) = struct

  type arith = A.t
  type affine = Aff.t

  type t =
    {
      x : A.t ;
      y : A.t ;
    }

  let make x y : t = {x; y}

  let ( % ) = make

  type point = t

  open A

  let orig = make zero zero

  let center {x;y} {x=x';y=y'} =
    let newx = if x > x' then x' +. (x-.x')/. two else x +. (x' -. x) /. two
    and newy = if y > y' then y' +. (y-.y')/. two else y +. (y' -. y) /. two
    in make newx newy

  let determinant a b c =
    (b.x -. a.x) *. (c.y -. a.y) -. (b.y -. a.y) *. (c.x -. a.x)

  let iso_barycenter pts =
    let rec aux pts sumx sumy nb =
      match pts with
      | [] -> make (sumx /. nb) (sumy /. nb)
      | h::tl -> aux tl (sumx +. h.x) (sumy +. h.y) (nb +. one)
    in aux pts zero zero zero

  let barycenter weighted_pts =
    let rec aux pts sumx sumy sumw =
      match pts with
      | [] -> make (sumx /. sumw) (sumy /. sumw)
      | (pt,w)::tl -> aux tl ((w*.pt.x) +. sumx) ((w*.pt.y) +. sumy) (w+.sumw)
    in aux weighted_pts zero zero zero

  let sq_distance ({x=a;y=b}) ({x=c;y=d}) =
    let diffX = a -. c and diffY = b -. d in
    (diffX *. diffX +. diffY *. diffY)

  let distance ({x=a;y=b}) ({x=c;y=d}) =
    sq_distance {x=a;y=b} {x=c;y=d}
    |> A.to_float
    |> sqrt
    |> A.of_float

  let x_coord p = p.x

  let y_coord p = p.y

  let scale_x p f = {p with x=p.x*.f}

  let scale_y p f = {p with y=p.y*.f}

  let translate dx dy ({x;y}) = make (x+.dx) (y+.dy)

  let transform aff p =
    let x, y = Aff.transform_point aff p.x p.y in {x; y}

  let point_reflection center p =
    translate (center.x -. p.x) (center.y -. p.y) center

  let rotate pivot p angle =
    let angle = A.to_float angle in
    let ca = cos angle |> A.of_float in
    let sa = sin angle |> A.of_float in
    let px = ca *. (p.x -. pivot.x) -. sa *. (p.y -. pivot.y) +. pivot.x
    and py = sa *. (p.x -. pivot.x) +. ca *. (p.y -. pivot.y) +. pivot.y
    in make px py

  let rotate_angle pivot p angle =
    let angle = A.to_float angle in
    let angle = angle |>  Math.deg_to_rad |> A.of_float in
    rotate pivot p angle

  let print fmt {x;y} = Format.fprintf fmt "{x=%a; y=%a}" A.pp_print x A.pp_print y
end
