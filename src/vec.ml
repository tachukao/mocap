type t = float * float * float

let map ~f (x, y, z) = f x, f y, f z

let iter ~f (x, y, z) =
  f x;
  f y;
  f z


let neg = map ~f:(fun x -> -.x)
let ( + ) (x1, y1, z1) (x2, y2, z2) = x1 +. x2, y1 +. y2, z1 +. z2
let ( - ) (x1, y1, z1) (x2, y2, z2) = x1 -. x2, y1 -. y2, z1 -. z2
let ( * ) (x1, y1, z1) (x2, y2, z2) = x1 *. x2, y1 *. y2, z1 *. z2
let ( / ) (x1, y1, z1) (x2, y2, z2) = x1 /. x2, y1 /. y2, z1 /. z2
let ( $+ ) a = map ~f:(fun x -> a +. x)
let ( $- ) a = map ~f:(fun x -> a -. x)
let ( $* ) a = map ~f:(fun x -> a *. x)
let ( $/ ) a = map ~f:(fun x -> a /. x)
let ( +$ ) v a = map ~f:(fun x -> x +. a) v
let ( -$ ) v a = map ~f:(fun x -> x -. a) v
let ( *$ ) v a = map ~f:(fun x -> x *. a) v
let ( /$ ) v a = map ~f:(fun x -> x /. a) v
let print (x, y, z) = Stdio.printf "(%f, %f, %f)\n%!" x y z
