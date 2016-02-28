type t = Constraint.t list

let make (cl:Constraint.t list) : t = cl

let contains cl p = List.for_all (fun e -> Constraint.contains e p) cl

let translate dx dy cl = List.map (fun e -> Constraint.translate dx dy e) cl

let intersection p1 p2 = List.rev_append p1 p2
