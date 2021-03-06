
module List = struct

  let rec last = function
    | [] -> raise (Failure "failure last")
    | [a] -> a
    | _::tl -> last tl

  let consr a b = b :: a

  let split_concat_sorted compare l =
    let rec aux acc l' =
      match l' with
      | [] | [_] -> l
      | h1 :: h2 :: t ->
        if compare h1 h2 > 0 then
          (h2 :: t) @ List.rev (h1 :: acc)
        else aux (h1 :: acc) (h2 :: t)
    in aux [] l

  (** List is sorted but not from the start *)

  let rec print_sep f sep fmt l =
    match l with
    | [] -> ()
    | [e] -> Format.fprintf fmt "%a" f e
    | h :: t ->
      Format.fprintf fmt "%a%s%a" f h sep (print_sep f sep) t

end
