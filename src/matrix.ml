open VectorSpace;;

type matrix = float array array;;

exception Matrix_incompatible;;

let transpose (m : matrix) =
  let n = Array.length m and p = Array.length (m.(0)) in
  let (mt : matrix) = Array.make_matrix p n 0. in
  for i = 0 to (p - 1) do
    for j = 0 to (n - 1) do
      mt.(i).(j) <- m.(j).(i)
    done;
  done;
  mt
;;

let mul (m1 : matrix) (m2 : matrix) =
  let n = Array.length m1 and p = Array.length m2 and q = Array.length (m2.(0)) in
  if p <> (Array.length (m1.(0))) then raise Matrix_incompatible;
  let (m : matrix) = Array.make_matrix n q 0. in
    for i = 0 to (n - 1) do
      for j = 0 to (q - 1) do
        for k = 0 to (p - 1) do
          m.(i).(j) <- m.(i).(j) +. (m1.(i).(k) *. m2.(k).(j))
        done;
      done;
    done;
    m
;;

let matrix_of_vectors (v : vector array) = (transpose v);;

(* return P_bc->b *)
let transition_matrix (base : vector array) = (matrix_of_vectors base);;
