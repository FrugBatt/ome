type vector = float array;;
type vector_space = {dimension: int; base: vector array; v_add: vector -> vector -> vector; s_mul: float -> vector -> vector};;

exception Vector_incompatible;;
exception Nil_vector;;

let space_dimension (e : vector_space) = Array.length (e.base);;

(* check vectors *)
let is_valid (e : vector_space) (v : vector) = Array.length v = (space_dimension e);;
let is_nil (v : vector) =
  let n = Array.length v and i = ref 0 in
  while (!i < n) && (v.(!i) = 0.) do
    incr i
  done;
  !i = n
;;
let is_collinear (v1 : vector) (v2 : vector) =
  let n = Array.length v1 in
  if is_nil v2 then true
  else let i0 = ref 0 in
    while v2.(!i0) = 0. do incr i0 done;
    let t = v1.(!i0) /. v2.(!i0) and i = ref 0 in
    while (!i < n) && (v1.(!i) = t *. v2.(!i0)) do incr i done;
    (!i = n)
;;

(* default operators *)
let vector_add (v1 : vector) (v2 : vector) =
  let n1 = Array.length v1 and n2 = Array.length v2 in
  if n1 <> n2 then raise Vector_incompatible;
  let (v : vector) = Array.make n1 0. in
  for i = 0 to (n1-1) do
    v.(i) <- v1.(i) +. v2.(i)
  done;
  v
;;

let scalar_multiply (l : float) (v : vector) =
  let n = Array.length v in
  let (r : vector) = Array.make n 0. in
  for i = 0 to (n-1) do
    r.(i) <- l *. v.(i)
  done;
  r
;;

let canonical_base dim =
  let (b : vector array) = Array.make_matrix dim dim 0. in
  for i = 0 to (dim - 1) do
    b.(i).(i) <- 1.
  done;
  b
;;

(* return a R^n vector space *)
let default_vector_space dim = {dimension= dim; base= (canonical_base dim); v_add= vector_add; s_mul= scalar_multiply};;
