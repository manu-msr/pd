(** Programación Declarativa
    Práctica 4
   ·Alfaro Mendoza Yoshua Ian
   ·Patlani Aguilar Luis Ángel 
   ·Soto Romero Manuel **)

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
