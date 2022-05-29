open Ome.EuclideanSpace;;

print_endline "Hello World!";;
let v1 : vector = [|1.;0.|];;
let v2 : vector = [|2.;0.|];;
print_float (canonical_scalar_prod v1 v2);;
