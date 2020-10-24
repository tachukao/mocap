open Base

type t = float * (float * float * float)

let _resign (a, (x, y, z)) = if Float.(a > 0.) then a, (x, y, z) else -.a, (-.x, -.y, -.z)
let ( + ) (a1, u1) (a2, u2) = a1 +. a2, Vec.(u1 + u2)
let ( - ) (a1, u1) (a2, u2) = a1 -. a2, Vec.(u1 - u2)

let dot_prod (a1, (x1, y1, z1)) (a2, (x2, y2, z2)) =
  (a1 *. a2) +. (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)


let sqr_norm q = dot_prod q q
let ( $* ) c (a, u) = c *. a, Vec.(c $* u)
let ( /$ ) (a, u) c = a /. c, Vec.(u /$ c)

let ( * ) (a1, (b1, c1, d1)) (a2, (b2, c2, d2)) =
  let a = (a1 *. a2) -. (b1 *. b2) -. (c1 *. c2) -. (d1 *. d2) in
  let u =
    ( (a1 *. b2) +. (b1 *. a2) +. (c1 *. d2) -. (d1 *. c2)
    , (a1 *. c2) -. (b1 *. d2) +. (c1 *. a2) +. (d1 *. b2)
    , (a1 *. d2) +. (b1 *. c2) -. (c1 *. b2) +. (d1 *. a2) )
  in
  a, u


let conj (a, v) = a, Vec.neg v

let of_sxyz (x, y, z) =
  let x, y, z = x /. 2., y /. 2., z /. 2. in
  let c1, c2, c3 = Float.(cos x, cos y, cos z) in
  let s1, s2, s3 = Float.(sin x, sin y, sin z) in
  ( (c1 *. c2 *. c3) +. (s1 *. s2 *. s3)
  , ( (s1 *. c2 *. c3) -. (c1 *. s2 *. s3)
    , (c1 *. s2 *. c3) +. (s1 *. c2 *. s3)
    , (c1 *. c2 *. s3) -. (s1 *. s2 *. c3) ) )


let rotate q v =
  let p = 0., v in
  let _, v = q * p * conj q in
  v


let print (a, (x, y, z)) = Stdio.printf "(%f, %f, %f, %f)\n%!" a x y z
