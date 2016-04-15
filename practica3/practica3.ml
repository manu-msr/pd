open String;;

(* Programación Declarativa
   Práctica 3 - OCaml 
   Alfaro Mendoza Yoshua Ian
   Patlani Aguilar Luis Ángel
   Manuel Soto Romero *)


(* Ejercicio 1 *)

(* Función que hace el trabajo de combinaciones. Esta función opera con
   flotantes pues la división entera no nos sirve. 
   combinaciones : float * float -> float *)
let rec combinaciones parnp =
   match parnp with
   |(n,0.0) -> 1.0
   |(n,p) -> (n/.p)*.(combinaciones (n -. 1.0,p -. 1.0))

(* Función que manda a llamar a combinaciones. Esta función recibe enteros
   y manda a llamar a una función que trabaja con flotantes. Al final vuelve a
   convertir el resultado a entero. 
   combinaciones1: int * int -> int*)
let combinaciones1 parnp = (int_of_float (combinaciones ((float_of_int (fst parnp)), (float_of_int (snd parnp)))))

(* Versión 2 de la función que encuentra el número de combinaciones. Al igual
   que en la implementación 2. Tenemos que hacer conversiones. 
   combinaciones2: int * int -> int *)
let rec combinaciones2 parnp = 
   let rec aux n p acc =
   if (p = 0.0) then
      (int_of_float acc)
   else
      (aux (n -. 1.0) (p -. 1.0) (acc *. (n /. p)))
   in aux (float_of_int (fst parnp)) (float_of_int (snd parnp)) 1.0


(* Ejercicio 3 *)

(* Funcion que devuelve una lista de cadenas asociadas con su longitud.*)
let rec longList ls = match ls with
|[] -> []
|s::xs -> let n = (length s) in
	if (n < 1) then longList xs
	else (s,n)::(longList xs)

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
   ejemplo mayor_color y menor_color) y como caso base toma el primer elemento 
   de la lista. La función de comparación debe ser binaria. 
   maximal : ('a -> 'a -> 'a) -> 'a list -> 'a *)
let rec maximal p l =
   match l with
   | [] -> failwith "Lista vacía"
   | (x::xs) -> it_list p x xs


(* Ejercicio 5 *)

(* Funcion que indica si dos palabras son anagramas.*)
let anagrams a b = let rec toList s i n = if i<n then (s.[i])::(toList s (i+1) n) else []
   and skip c s = match s with
      |[] -> []
      |x::xs -> if c=x then xs else x::(skip c xs)
   and aux u v = if (List.length u)<>(List.length v) then false
      else match u with
      |[] -> true
      |x::xs -> aux xs (skip x v)
   in aux (toList a 0 (length a)) (toList b 0 (length b))
	and skip c s = match s with
		|[] -> []
		|x::xs -> if c=x then xs else x::(skip c xs)
	and aux u v = if (List.length u)<>(List.length v) then false
		else match u with
		|[] -> true
		|x::xs -> aux xs (skip x v)
	in aux (toList a 0 (length a)) (toList b 0 (length b))


(* Ejercicio 6 *)

(* Construccion de arboles binarios.*)
type 'a btree = Leaf of 'a | Node of ('a btree * 'a btree)
(*6.1 Tipo de dato direccion para recorridos sobre arboles binarios.*)
type direction = L | R
(*6.2 Funcion que devuelve el arbol generado por una lista de direcciones.*)
let rec camino l t = match t with
   |(Leaf h) as hoja -> hoja
   |(Node (left,rigth)) as tree -> match l with
      |[] -> tree
      |L::xs -> camino xs left
      |R::xs -> camino xs rigth
(*Construccion de un arbol auxiliar.*)
let t = Node (Node (Leaf 3, Leaf 5), Node (Node (Leaf 2, Leaf 1), Leaf 6));;
	|(Leaf h) as hoja -> hoja
	|(Node (left,rigth)) as tree -> match l with
		|[] -> tree
		|L::xs -> camino xs left
		|R::xs -> camino xs rigth
(*Construccion de un arbol auxiliar.*)
let t = Node (Node (Leaf 3, Leaf 5), Node (Node (Leaf 2, Leaf 1), Leaf 6));;


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
