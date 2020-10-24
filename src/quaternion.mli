type t = float * (float * float * float)

val _resign : t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( $* ) : float -> t -> t
val ( /$ ) : t -> float -> t
val dot_prod : t -> t -> float
val sqr_norm : t -> float
val conj : t -> t
val of_sxyz : Vec.t -> t
val rotate : t -> Vec.t -> Vec.t
val print : t -> unit
