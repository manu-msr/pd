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

let rec obtenCola l =
   match l with
   | [] -> failwith "La lista está vacía"
   | [x] -> x
   | x::xs -> obtenCola xs

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

module Deque:DEQUE =
struct
   type 'a t = 'a list ref

   let getHead xs =
      match !xs with
      | [] -> failwith "La cola está vacía"
      | x::xs -> x

   let getTail xs =
      match !xs with
      | [] -> failwith "La cola está vacía"
      | x::xs -> obtenCola xs

   let enqueueHead e c = 
      c := e::!c

   let enqueueTail e c = 
      c := !c@[e]

   let dequeueHead c = 
      match !c with
      | [] -> failwith "La cola está vacía"
      | x::xs -> c := xs ; x

   let dequeueTail c = 
      match !c with
      | [] -> failwith "La cola está vacía"
      | x::xs -> obtenCola xs
end