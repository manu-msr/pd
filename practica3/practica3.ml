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
   |(n,p) -> (n/.p)*.(combinaciones (n -. 1.0,p -. 1.0))

(* Función auxiliar que obtiene el número de combinaciones con Recursión de 
   Cola, usa un acumulador. *)
let rec combs n p acc =
   if p = 0.0 then
      acc
   else
      (combs (n -. 1.0) (p -. 1.0) (acc *. (n /. p)))

(* Versión 2 de la función que encuentra el número de combinaciones. *)
let rec combinaciones2 parnp =
   match parnp with
   | (n, p) -> (combs n p 1.0);;

(* Ejercicio 4 *)

(* Función it_list que en realidad es foldl.
   it_list : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec it_list f e l =
   match l with
   | [] -> e
   | (a::l) -> it_list f (f e a) l

(* Para probar la función maximal, usamos el tipo color y definimos dos 
   funciones de comparación para colores. *)
type color =
   | Rojo
   | Verde
   | Azul

(* El orden de mayor a menor en colores sería Rojo, Verde Azul. Esta función se
   encarga de, dados dos colores, devolver el mayor.
   mayor_color : color -> color -> color *)
let mayor_color c1 c2 =
   match c1 with
   | Rojo -> Rojo
   | Verde -> if c2 = Rojo then Rojo else Verde
   | Azul -> c2

(* El orden de menor a mayor en colores sería Azul, Verde Rojo. Esta función se
   encarga de, dados dos colores, devolver el menor. 
   menor_color : color -> color -> color *)
let menor_color c1 c2 =
   match c1 with
   | Rojo -> c2
   | Verde -> if c2 = Azul then Azul else Verde
   | Azul -> Azul

(* Función maximal, que llama a it_list con la función de comparación, (por 
   ejemplo compara) y como caso base toma el primer elemento de la lista. *)
let rec maximal p l =
   match l with
   | [] -> failwith "Lista vacía"
   | (x::xs) -> it_list p x xs

(* Ejercicio 7. *)
type exp =
   | Const of int
   | X
   | Sum of exp * exp
   | Prod of exp * exp
   | Div of exp * exp
   | Exp of exp * int

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
   | Exp (b,p) -> "(" ^ (str_of_exp b) ^ "^" ^ (string_of_int p) ^ ")"

(* Función que toma una expresión y devuelve su derivada. 
   derive : exp -> exp *)
let rec derive exp =
   match exp with
   | Const n -> (Const 0)
   | X -> (Const 1)
   | Sum (i,d) -> (Sum (derive i, derive d))
   | Prod (i,d) -> (Sum ((Prod (derive i, d)), (Prod (i, derive d))))
   | Div (i,d) -> (Div ((Prod ((Sum ((Prod (derive i,d)),(Prod (i,(derive d))))),(Const (-1)))),(Exp (d,2))))
   | Exp (b,p) -> (Prod ((Exp ((Prod ((Const p),b),p-1)), (derive b))))
   