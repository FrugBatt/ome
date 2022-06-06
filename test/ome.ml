open Printf;;
open Ome.EuclideanSpace;;
open Ome.VectorSpace;;

let euc_space = default_euclidean_space 2;;
let v1 : vector = [|1.;0.|];;
let v2 : vector = [|3.;2.|];;
let v : vector array = gram_schmidt euc_space [|v1;v2|];;

printf "(%f,%f) (%f,%f)" v.(0).(0) v.(0).(1) v.(1).(0) v.(1).(1);;
