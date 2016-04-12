(* Práctica 3 *)


(* Ejercicio 1 *)
let rec combinaciones parnp =
	match parnp with
	|(n,0.0) -> 1.0
	|(n,p) -> (n/.p)*.(combinaciones (n -. 1.0,p -. 1.0));;

let rec combinaciones2 parnp =
	match parnp with
	| (n, p) -> (combs n p 1.0);;

let rec combs n p acc =
	if p = 0.0 then
		acc
	else
		(combs (n -. 1.0) (p -. 1.0) (acc *. (n /. p)));;