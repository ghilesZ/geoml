(** solve a b c returns the list of the solutions for
    ax² + bx + c = 0 *)
let solve a b c =
  let delta = b*.b -. 4. *. a *. c in
  if delta < 0. then []
  else 
    let racine_delta = sqrt delta in
    let sol1 = (-.b -. racine_delta) /. (2.*.a) in
    if delta = 0. then [sol1]
    else let sol2 = (-.b +. racine_delta) /. (2.*.a) in [sol1;sol2]
