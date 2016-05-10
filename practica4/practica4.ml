(** Programación Declarativa
    Práctica 4
   ·Alfaro Mendoza Yoshua Ian
   ·Patlani Aguilar Luis Ángel 
   ·Soto Romero Manuel **)

(* Función que itera una función. Esta función toma como argumento una función y
   un entero y regresa un arreglo de tamaño n donde el elemento i del arreglo es
   f i.
   tabular : (int -> 'a) -> int -> 'a array *)
let tabular f n = 
	let arreglo = Array.make n (f n) in
		for i = 0 to (n-1) do
			arreglo.(i) <- f i
		done;
	arreglo