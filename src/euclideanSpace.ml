open VectorSpace;;

type euclidean_space = {space: vector_space; s_prod: vector -> vector -> float};;

let norm (es : euclidean_space) (v : vector) = sqrt (es.s_prod v v);;
let normalize (es : euclidean_space) (v : vector) = es.space.s_mul (1. /. (norm es v)) v;;

(* default operator *)
let canonical_scalar_prod (v1 : vector) (v2 : vector) =
  let n1 = Array.length v1 and n2 = Array.length v2 in
    if n1 <> n2 then raise Vector_incompatible;
    let s = ref 0. in
      for i = 0 to (n1-1) do
        s := (v1.(i) *. v2.(i)) +. !s
      done;
     !s 
;;

(* return a R^n euclidean space *)
let default_euclidean_space dim = {space= (default_vector_space dim); s_prod= canonical_scalar_prod};;

(* apply the gram-schmidt algorithm to a free vector family and normalize *)
let gram_schmidt (es : euclidean_space) (v : vector array) =
  let n = Array.length v in
  let (gs : vector array) = Array.copy v in
  for i = 0 to (n - 1) do
    for j = 0 to (i - 1) do
      let beta = -. (es.s_prod v.(i) gs.(j)) in
        gs.(i) <- (es.space.v_add gs.(i) (es.space.s_mul beta gs.(j)))
    done;
    gs.(i) <- (normalize es gs.(i));
  done;
  gs
;;

(* sub vector space *)
(* let orthogonal_subspace (es : euclidean_space) (n : vector) = *)
  (* if is_nil n then raise Nil_vector; *)
  (* if not (id_valid es.space n) then raise Vector_incompatible; *)
  
