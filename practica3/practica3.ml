open String;;

(* Programación Declarativa
   Práctica 3 - OCaml 
   Alfaro Mendoza Yoshua Ian
   Patlani Aguilar Luis Ángel
   Manuel Soto Romero *)

(* Ejercicio 1 
   Función que calcula el número de combinaciones p en n.
   combinaciones : (float * float) -> float *)
let rec combinaciones parnp =
   match parnp with
   |(n,0.0) -> 1.0
   |(n,p) -> (n/.p)*.(combinaciones (n -. 1.0,p -. 1.0));;

(* Función auxiliar que obtiene el número de combinaciones con Recursión de 
   Cola, usa un acumulador. *)
let rec combs n p acc =
   if p = 0.0 then
      acc
   else
      (combs (n -. 1.0) (p -. 1.0) (acc *. (n /. p)));;

(* Versión 2 de la función que encuentra el número de combinaciones. *)
let rec combinaciones2 parnp =
   match parnp with
   | (n, p) -> (combs n p 1.0);;

(* Ejercicio 4 *)
let rec it_list f e l =
   match l with
   | [] -> e
   | (a::l) -> it_list f (f e a) l

let rec maximal p l =
   match l with
   | [] -> failwith "Lista vacía"
   | (x::xs) -> (it_list p (p x (maximal p xs)) xs);;

(* Ejercicio 4. *)
type exp =
   | Const of int
   | X
   | Sum of exp * exp
   | Prod of exp * exp
   | Div of exp * exp
   | Exp of exp * int;;

(* Función que toma una expresión y la convierte en cadena. Agregamos paréntesis
   para hacer la expresión más entendible.
   str_of_exp : exp -> string *)
let rec str_of_exp exp =
   match exp with
   | Const n -> (string_of_int n)
   | X -> "X"
   | Sum (i,d) -> "(" ^ (str_of_exp i) ^ " + " ^ (str_of_exp d) ^ ")" 
   | Prod (i,d) -> "(" ^ (str_of_exp i) ^ " * " ^ (str_of_exp d) ^ ")"
   | Div (i,d) -> "(" ^ (str_of_exp i) ^ " / " ^ (str_of_exp d) ^ ")"
   | Exp (b,p) -> "(" ^ (str_of_exp b) ^ "^" ^ (string_of_int p) ^ ")";; 

(* Función que toma una expresión y devuelve su derivada. 
   derive : exp -> exp *)
let rec derive exp =
   match exp with
   | Const n -> (Const 0)
   | X -> (Const 1)
   | Sum (i,d) -> (Sum (derive i, derive d))
   | Prod (i,d) -> (Sum ((Prod (derive i, d)), (Prod (i, derive d))))
   | Div (i,d) -> (Div ((Prod ((Sum ((Prod (derive i,d)),(Prod (i,(derive d))))),(Const (-1)))),(Exp (d,2))))
   | Exp (b,p) -> (Const 0);;
