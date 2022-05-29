type vector = float array;;
type scalar_prod = vector -> vector -> float;;
type space = int;;

exception Vector_incompatible;;

(* Check if a vector is in space *)
let is_in_space (e : space) (v : vector) = Array.length v = e;;


let canonical_scalar_prod (v1 : vector) (v2 : vector) =
  let n1 = Array.length v1 and n2 = Array.length v2 in
    if n1 <> n2 then raise Vector_incompatible;
    let s = ref 0. in
      for i = 0 to (n1-1) do
        s := (v1.(i) *. v2.(i)) +. !s
      done;
      !s
;;
