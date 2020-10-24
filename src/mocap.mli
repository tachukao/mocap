open Base
module Vec : module type of Vec
module Quaternion : module type of Quaternion

type 'a xyz = 'a * 'a * 'a

val deg_to_rad : float -> float

module Joint : sig
  type t =
    { name : string
    ; length : float
    ; direction : float xyz
    ; axis : float xyz
    ; dof : (float * float) option xyz
    ; q : Quaternion.t
    }
end

module Coord : sig
  type t =
    { position : float xyz
    ; q : Quaternion.t
    }

  val process
    :  root:t
    -> joints:(string, Joint.t) Base.Hashtbl.t
    -> hierarchy:(string, string list) Base.Hashtbl.t
    -> (string, float list) Base.Hashtbl.t
    -> (string, Vec.t) Base.Hashtbl.t

  val links
    :  hierarchy:(string, string list) Base.Hashtbl.t
    -> (string, Vec.t) Base.Hashtbl.t
    -> ((string * Vec.t) * (string * Vec.t)) list
end

val read_asf
  :  string
  -> Coord.t * (string, Joint.t) Base.Hashtbl.t * (string, string list) Base.Hashtbl.t

val read_amc : string -> (string, float list) Base.Hashtbl.t list
