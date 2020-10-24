type t = float * float * float

val map : f:(float -> float) -> t -> t
val iter : f:(float -> unit) -> t -> unit
val neg : t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( $+ ) : float -> t -> t
val ( $- ) : float -> t -> t
val ( $* ) : float -> t -> t
val ( $/ ) : float -> t -> t
val ( +$ ) : t -> float -> t
val ( -$ ) : t -> float -> t
val ( *$ ) : t -> float -> t
val ( /$ ) : t -> float -> t
val print : t -> unit
