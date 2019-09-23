module type T = sig
  type t

  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val ( < )  : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > )  : t -> t -> bool
  val ( >= ) : t -> t -> bool

  val neg : t -> t

  val to_float  : t -> float
  val of_float  : float -> t
  val to_rat    : t -> Q.t
  val of_rat    : Q.t -> t
  val to_string : t -> string

  val pp_print : Format.formatter -> t -> unit

  val zero : t
  val one : t
  val minus_one : t
  val two : t
end

module Float = (struct
  type t = float

  let ( +. ) = ( +. )
  let ( -. ) = ( -. )
  let ( *. ) = ( *. )
  let ( /. ) = ( /. )

  let ( > ) = ( > )
  let ( < ) = ( < )
  let ( >= ) = ( >= )
  let ( <= ) = ( <= )

  let neg : t -> t = (~-.)

  external to_float : t -> float = "%identity"
  external of_float : float -> t = "%identity"
  let to_rat = Q.of_float
  let of_rat = Q.to_float
  let to_string = string_of_float

  let pp_print fmt x = Format.fprintf fmt "%f" x

  let zero = 0.
  let one  = 1.
  let minus_one  = 1.
  let two  = 2.
end : T)

module Rat = (struct
  type t = Q.t

  let ( +. ) = Q.add
  let ( -. ) = Q.sub
  let ( *. ) = Q.mul
  let ( /. ) = Q.div

  let ( > ) = Q.gt
  let ( < ) = Q.lt
  let ( >= ) = Q.geq
  let ( <= ) = Q.leq

  let neg = Q.neg

  let to_float = Q.to_float
  let of_float = Q.of_float
  external to_rat : t -> Q.t = "%identity"
  external of_rat : Q.t -> t = "%identity"
  let to_string = Q.to_string

  let pp_print fmt x = Format.fprintf fmt "%a" Q.pp_print x

  let zero = Q.zero
  let one  = Q.one
  let minus_one  = Q.minus_one
  let two  = Q.of_int 2
end : T)
