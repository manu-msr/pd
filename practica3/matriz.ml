(* Modulo para la representacion de matrices *)

(* Se usan las operaciones de vectores. *)
(* Una matriz tiene m renglones y n columnas y sus elementos son flotantes.*)

module Matriz = 
  struct 
  let make_m m n = 
    let matriz = Array.make n [||] in
    for i = 0 to (n-1) do
      matriz.(i) <- Array.make m 0.;
    done;
    matriz
  let rows a = Array.length a
  let cols a = Array.length a.(0)
  let str_of_m a = 
    let s = ref "" in
    let m = rows a in
    let n = cols a in 
    for i = 0 to (m-1) do
      for j = 0 to (n-1) do
	s := !s ^ " " ^ string_of_float a.(i).(j)
      done;
      s := !s ^ "\n"
    done;
    !s    
  let print_m m = print_string (str_of_m m)
  end