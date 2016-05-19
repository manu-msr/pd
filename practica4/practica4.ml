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

(* Ejercicio 5. *)
module type TREE =
sig
   type t
   val create: unit -> t
   val add: int -> t -> unit
   val mem: int -> t -> bool
   val remove: int -> t -> unit
   val size: t -> int
   val height: t -> int
   val getRoot: t -> int
   val bfs: t -> unit
end

type node = E | N of t * int * t
and t = node ref

module BBTree:TREE =
struct
   type t = node ref

   (* Función que crea un árbol vacío *)
   let getRoot bb =
      match !bb with
      | E -> failwith "Árbol vacío"
      | N(i,e,d) -> e

   let rec bfs bb =
      match !bb with
      | E -> (print_string " ")
      | N(i,e,d) -> print_int e ; let a = 1 in
         bfs i; bfs d

   let create () = ref E

   let rec addaux n bb =
      match bb with
      | E -> N (ref E, n, ref E)
      | N(i,e,d) -> 
         if n < e
         then
            N (ref (addaux n !i), e, d)
         else
            N (i, e, ref(addaux n !d))

   (* Función que agrega un elemento al árbol *)
   let rec add n bb =
      bb := addaux n !bb

   let rec mem n bb =
      match !bb with
      | E -> false
      | N(i,e,d) ->
         if e = n
         then
            true
         else
            (mem n i) || (mem n d)

   let remove n bb = ()
   
   let rec size bb =
      match !bb with
      | E -> 0
      | N(i,e,d) -> 1 + (size i) + (size d)

   let rec height bb =
      match !bb with
      | E -> -1
      | N(i,e,d) -> 1 + (max (height i) (height d))
end

(* Ejercicio 7. *)

(* Interfaz para trabajar con bicolas. Una bicola tiene que implementar las
   siguientes funciones. *)
module type DEQUE = 
sig
   type 'a t
   val create : unit -> 'a t
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

   (* Función que crea una bicola vacía *)
   let create () = ref [| |]

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
      if (Array.length !bc = 0) 
      then 
         bc := Array.make 1 e
      else 
         let a = Array.make ((Array.length !bc)+1) e in
            for i = 0 to (Array.length !bc)-1 do
               a.(i+1) <- !bc.(i)
            done;
            bc := a 

   (* Función que agrega un elemento al final de la cola *)
   let enqueueTail e bc = 
      if (Array.length !bc = 0) 
      then 
         bc := Array.make 1 e
      else 
         let a = Array.make ((Array.length !bc)+1) e in
            for i = 0 to (Array.length a)-2 do
               a.(i) <- !bc.(i)
            done;
            bc := a 

   (* Función que elimina el primer elemento de la cola y lo devuelve. *)
   let dequeueHead bc =
      if (Array.length !bc = 0)
      then
         failwith "La bicola está vacía"
      else 
         let a = Array.make ((Array.length !bc)-1) !bc.(0) in
            let cabeza = !bc.(0) in
               for i = 0 to (Array.length !bc)-2 do
                  a.(i) <- !bc.(i+1)
               done;
               bc := a; cabeza

   (* Función que elimina el último elemento de la colay lo devuelve. *)
   let dequeueTail bc = 
      if (Array.length !bc = 0)
      then
         failwith "La bicola está vacía"
      else
         let a = Array.make ((Array.length !bc)-1) !bc.(0) in
            let cola = !bc.((Array.length !bc)-1) in
               for i = 0 to (Array.length a)-1 do
                  a.(i) <- !bc.(i)
               done;
               bc := a; cola
end

(* Ejercicio 8 *)

(* Función recursiva que calcula el menor costo para multiplicar las matrices
   con las dimensiones dadas, a partir de esquematizar el caso de 3 matrices,
   es decir, el minimo entre la asociacion del lado izquierdo y del derecho.
   La lista dims debe contener al menos 3 dimensiones.
   val min_cost : int list -> int *)
let rec min_cost dims = match dims with
| a::b::c::d -> match d with
      | [] -> a*b*c
      | x::xs -> let rev = List.rev dims in let tail = List.tl rev in
         let f = List.hd rev and e = List.hd tail
         in min (a*b*f + (min_cost (b::c::d)))
               (a*e*f + (min_cost (List.rev tail)))

(*Tabla hash para almacenar los calculos de los costos minimos.*)
let mc = Hashtbl.create 500

(* Función memoizada para obtener el costo minimo para multiplicar
   las matrices con las dimensiones dadas, siguiendo la estrategia
   recursiva y guardando los casos simples que posiblemente sean
   reutilizados, para que no sean calculados de nuevo.
   La lista dims debe contener al menos 3 dimensiones.
   val mem_cost : int list -> int *)
let rec mem_cost dims = try Hashtbl.find mc dims
   with Not_found -> match dims with
      | a::b::c::d -> let r = match d with
            | [] -> a*b*c
            | x::xs -> let rev = List.rev dims in let tail = List.tl rev in
               let f = List.hd rev and e = List.hd tail
               in min (a*b*f + (mem_cost (b::c::d)))
                     (a*e*f + (mem_cost (List.rev tail)))
            in Hashtbl.add mc dims r;r