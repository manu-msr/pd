open List;;

(** Programación Declarativa
    Práctica 4
   ·Alfaro Mendoza Yoshua Ian
   ·Patlani Aguilar Luis Ángel 
   ·Soto Romero Manuel **)

(* Ejercicio 1. *)

(* Variable global auxiliar, usada por la funcion crack, para relacionar
   los valores de sus parametros y modificar su valor de manera ordenada. *)
let crack_v = ref [];;

(* Función que emplea elementos imperativos con los que no se satisface que:
   List.map crack (List.rev xs) ≡ List.rev (List.map crack xs), donde
   crack : '_a -> '_a list *)
let crack v = crack_v:= !crack_v @ [v]; !crack_v;;

(* Ejercicio 2. *)

(* Función que itera una función. Esta función toma como argumento una 
   función y un entero y regresa un arreglo de tamaño n donde el elemento i del 
   arreglo es f i.
   tabular : (int -> 'a) -> int -> 'a array *)
let tabular f n = 
   let arreglo = Array.make n (f n) in
      for i = 0 to (n-1) do
         arreglo.(i) <- f i
      done;
   arreglo

(* Ejercicio 3.1 *)

(* Función que toma dos streams y regresa un stream de pares, donde la
   primer entrada de un par es elemento del primer stream y la segunda
   del segundo stream.
   zip_streams : 'a Stream.t -> 'b Stream.t -> ('a * 'b) Stream.t *)
let zip_streams sa sb = 
  let rec nextAB i = 
  try 
    Some (Stream.next sa, Stream.next sb)
  with
    Stream.Failure -> None 
  in Stream.from nextAB;;

(* Ejercicio 3.2 *)

(* Función que toma dos streams y regresa un stream que alterna los
   elementos de ambos streams haciendo uso del contador del Stream.
   merge_streams : 'a Stream.t -> 'a Stream.t -> 'a Stream.t *)
let merge_streams sa sb = 
  let rec next i = 
  try    
   if (i mod 2)=0
   then Some (Stream.next sa)
   else Some (Stream.next sb)
  with 
    Stream.Failure -> None 
  in Stream.from next;;

(* Ejercicio 7. *)

(* Función auxiliar que obtiene el último elemento de una lista. *)
let rec obtenCola l =
   match l with
   | [] -> failwith "La lista está vacía"
   | [x] -> x
   | x::xs -> obtenCola xs

(* Función auxiliar que devuelve una lista sin el último elemento. *)
let rec quitaUltimo l =
   match l with
   | [] -> []
   | [x] -> []
   | x::xs -> x::quitaUltimo xs

(* Interfaz para trabajar con bicolas. Una bicola tiene que implementar las
   siguientes funciones. *)
module type DEQUE = 
sig
   type 'a t
   val getHead : 'a t -> 'a
   val getTail : 'a t -> 'a
   val enqueueHead : 'a -> 'a t -> unit
   val enqueueTail : 'a -> 'a t -> unit
   val dequeueHead : 'a t -> 'a
   val dequeueTail : 'a t -> 'a
end

(* Modulo que implementa una bicola. Se implementan por medio de arreglos. *)
module Deque:DEQUE =
struct
   type 'a t = 'a array ref

   (* Función que obtiene la cabeza de la cola *)
   let getHead bc =
      try 
         !bc.(0) 
      with
         Invalid_argument a -> failwith "La bicola está vacía"

   (* Función que obtiene el rabo de la cola *)
   let getTail bc =
      try
         !bc.((Array.length !bc)-1)
      with
         Invalid_argument a -> failwith "La bicola está vacía"

   (* Función que agrega un elemento al inicio de la cola *)
   let enqueueHead e bc = 
      let a = Array.make ((Array.length !bc)+1) e in
         for i = 1 to (Array.length a)-1 do
            a.(i) <- !bc.(i)
         done;
         bc := a 

   let enqueueTail e bc = 
      let a = Array.make ((Array.length !bc)+1) e in
         for i = 0 to (Array.length !bc)-1 do
            a.(i) <- !bc.(i)
         done;
         bc := a

   let dequeueHead bc = 
      !bc.(0)

   let dequeueTail bc = 
      !bc.(0)
end
