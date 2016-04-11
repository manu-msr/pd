(* Práctica 3 *)

let rec combinaciones parnp =
	match parnp with
	|(n,0.0) -> 1.0
	|(n,p) -> (n/.p)*.(combinaciones (n -. 1.0,p -. 1.0));;