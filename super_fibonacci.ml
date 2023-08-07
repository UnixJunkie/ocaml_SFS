
let pi = 4.0 *. (atan 1.0)
let two_pi = 2.0 *. pi

(* both constants are from p4 of the paper *)
let phi = sqrt 2.0
let psi = 1.533751168755204288118041

(* core function from Algorithm 1 on p4 of
   Alexa_CVPR_2022_SO3sampling.pdf *)
let super_fibonacci n i =
  let s = (float i) +. 0.5 in
  let t = s /. n in
  let d = two_pi *. s in
  let c_r = sqrt t in
  let c_R = sqrt (1.0 -. t) in
  let alpha = d /. phi in
  let beta  = d /. psi in
  (c_r *. sin alpha,
   c_r *. cos alpha,
   c_R *. sin beta,
   c_R *. cos beta)

let sample n =
  Array.init n (super_fibonacci (float n))
;;
