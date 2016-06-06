open Printf

(* Programación Declarativa
   Proyecto 2 - PageRank
   Manuel Soto Romero <manu@ciencias.unam.mx> *)

(* Record para almacenar las páginas en formato JSON.
   Una página está formada por una url, un id, una lista de links (para formar
   la gráfica), una lista de palabras asociadas (para hacer búsquedas) y el 
   valor de PR de cada una. *)
type pagina = {
    url : string;
    id : int;
    links : int list;
    texto : string list;
    mutable pagerank : float;
};;

(* Función que toma el nombre de un archivo y devuelve una lista de las líneas
   asociadas al mismo.
   leer_archivo : string -> string list *)
let leer_archivo archivo =
    let file = open_in archivo in
        let lista = ref [] in
            try
                while true do
                    let linea = input_line file in
                        lista := linea :: !lista
                done; List.rev !lista
            with End_of_file ->
                close_in file; List.rev !lista;;

(* Conviertne un carácter a cadena. 
   char_a_cadena : char -> string *)
let char_a_cadena c = "" ^ Char.escaped c;;

(* Función que toma una cadena y la descompone en una lista de caracteres para
   procesarlos individualmente.
   cadena_a_lista string -> char list *)
let cadena_a_lista cadena = 
    let rec cal i l =  if i <  0 then l else cal (i-1) (cadena.[i] :: l) in
        cal (String.length cadena - 1) [];;

(* Función que verifica si una cadena está contenida en otra. *
   contiene : string -> string -> bool *)
let contiene s1 s2 =
    try
        let len = String.length s2 in
            for i = 0 to String.length s1 - len do
                if String.sub s1 i len = s2 then raise Exit
            done;
        false
    with Exit -> true

(* Función que separa los elementos de la lista, primero quitamos los 
   identificadores pues no nos porporcionan información relevante. Después 
   separamos cada uno de los campos con un 'pipe' para procesarlos más adelante.
   cadena_json : char list -> string *)
let cadena_json lista = 
    let rec caj c l = 
        match l with
        | [] -> c
        | x::xs when x == '{' -> caj c xs
        | x::xs when compare c "\"url\":" == 0 -> caj "" xs
        | x::xs when contiene c ", \"id\":" -> caj ((String.sub c 0 ((String.length c) - 7)) ^ "|") xs
        | x::xs when contiene c ", \"links\":" -> caj ((String.sub c 0 ((String.length c) - 10)) ^ "|") xs
        | x::xs when contiene c ", \"texto\":" -> caj ((String.sub c 0 ((String.length c) - 10)) ^ "|") xs
        | x::xs when x == '}' -> c
        | x::xs -> caj (c ^ (char_a_cadena x)) xs in
            caj "" lista;;

(* Función que dada la cadena separada por pipes generada por la función 
   cadena_json, devuelve una lista con cada campo como elemento. 
   json_lista : string -> string list *)
let json_lista cadena = 
    let rec jl c s l =
        match c with
        | [] -> l@[s]
        | x::xs when x == '|' -> (jl xs " " (l@[s]))
        | x::xs -> jl xs (s ^ (char_a_cadena x)) l
    in jl (cadena_a_lista cadena) "" [];;

(* Función auxiliar que convierte una cadena representando una lista de enteros
   en la lista que representa.
   cadena_listaEntera : string -> int *)
let cadena_listaEntera cadena = 
    let rec cle c l =
        match c with
        | [] -> l
        | x::xs when x = ' ' -> cle xs l
        | x::xs when x = '[' -> cle xs l
        | x::xs when x = ',' -> cle xs l
        | x::xs when x = ']' -> l
        | x::xs -> cle xs (l@[(int_of_string (char_a_cadena x))])
    in cle (cadena_a_lista cadena) [];;

(* Función auxiliar que convierte una cadena representando una lista de cadenas
   en la lista que representa.
   cadena_listaCadenas : string -> string list *)
let cadena_listaCadenas cadena =
    let rec clc c s l =
        match c with
        | [] -> l@[s]
        | x::xs when x == '[' -> clc xs s l
        | x::xs when x == '\"' -> clc xs s l
        | x::xs when x == ',' -> (clc xs "" (l@[String.trim s]))
        | x::xs when x == ']' -> l@[String.trim s]
        | x::xs -> clc xs (s ^ (char_a_cadena x)) l
    in clc (cadena_a_lista cadena) "" [];;

(* Función que dada la lista generada por la función json_lista construye el
   record 'pagina' para usarlo en el cálculo de page rank. *
   listaJson_a_pagina : string list -> pagina *)
let rec listaJson_a_pagina lista = 
    {url = (List.nth lista 0); id = (int_of_string (String.trim (List.nth lista 1))); 
    links = (cadena_listaEntera (List.nth lista 2)); texto = (cadena_listaCadenas (List.nth lista 3)); pagerank = 0.0};;

(* Función que dado un elemento en formato JSON, lo convierte al record que
   representa la página correspondiente. 
   procesa_elemento : string -> pagina*)
let procesa_elemento e =
    (listaJson_a_pagina (json_lista (cadena_json (cadena_a_lista e))));;

(* Función que se encarga de leer el archivo y por medio de map generar el 
   record correspondiente a cada miembro del formato JSON. 
   genera_paginas : string -> pagina list*)
let rec genera_paginas archivo =
    List.map (procesa_elemento) (leer_archivo archivo);;


(* Función que construye una matriz de nxn y la inicializa con el valor v. 
   crea_matriz: int -> int -> float array array*)
let crea_matriz n v =
    let matriz = Array.make n [||] in
        for i = 0 to n - 1 do
            matriz.(i) <- Array.make n (float_of_int v);
        done;
    matriz;;

(* Función que construye la gráfica de adyacencias.
   construye_grafica : pagina list -> float array array -> float array array *)
let construye_grafica paginas matriz =
    for i = 0 to (List.length paginas) - 1 do
        for j = 0 to (List.length (List.nth paginas i).links) - 1 do
            let n = (List.length (List.nth paginas i).links) in
                matriz.(i).((List.nth (List.nth paginas i).links) j) <- 1. /. (float_of_int n);
        done;
    done;
    matriz;;

(* Función que traspone una matriz de nxn. 
   traspuesta : float array array -> int -> float array array*)
let traspuesta matriz n =
    let traspuesta = crea_matriz n 0 in
        for i = 0 to n - 1 do
            for j = 0 to n -1 do
                traspuesta.(i).(j) <- matriz.(j).(i);
            done;
        done;
    traspuesta;;

(* Función que realiza el producto por escalar de una matriz de nxn. 
   producto_escalar : float array array -> float -> int -> float array array *)
let producto_escalar matriz escalar n =
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            matriz.(i).(j) <- matriz.(i).(j) *. escalar;
        done;
    done;
    matriz;;

(* Función que realiza la suma de dos matrices de n x n.
   suma : float array array -> float array array -> int -> float array array*)
let suma a b n =
    let suma = crea_matriz n 0 in
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                suma.(i).(j) <- a.(i).(i) +. b.(i).(j);
            done;
        done;
    suma;;

(* Función que multiplica un renĺón de la matriz por un vector.
   producto : float array -> float list -> int -> float *)
let producto vector1 vector2 n =
    let suma = ref 0. in
        for i = 0 to n-1 do
            suma := !suma +. (vector1.(i) *. (List.nth vector2 i));
        done;
    !suma;;

(* Función que calcula el nuevo valor de pagerank. 
   nuevoPR : float array array -> float list -> int -> float list *)
let nuevoPR g p n =
    let pr = ref [] in
        for i = 0 to (Array.length g) - 1 do
            pr:=  !pr@[(producto g.(i) p n)];
        done;
    !pr;;

(* Función para comparar dos listas. 
   compareVs : 'a list -> 'a list -> bool*)
let rec compareVs l1 l2 = 
    match l1, l2 with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | x::xs, y::ys -> x = y && compareVs xs ys

(* Función que revisa los valores de PR hasta que se estabilizan. 
   float array array -> float list -> int -> float list*)
let rec revisa g p n =
    let gp = nuevoPR g p n in
        if compareVs gp p = false then gp else revisa g p n;;

(* Función que implementa el algoritmo de PageRank 
   page_rank : float array array -> float array array -> float list -> float -> 
   float -> int -> float list *)
let page_rank mt e p s t n =
    let izquierdo = (producto_escalar mt s n) in
        let derecho = (producto_escalar e (t /. (float_of_int n)) n) in
            let g = (suma izquierdo derecho n) in
                revisa g p n;;

(* Función que prepara todos los valores del algoritmo. 
   inicia : pagina list -> float list *)
let inicia a =
    let b = crea_matriz (List.length a) 0 in
        let m = construye_grafica a b in
            let mt = traspuesta m (List.length a) in
                let e = crea_matriz (List.length a) 1 in
                    let n = (float_of_int (List.length a)) in
                        let l = ref [] in
                            for i = 0 to (List.length a) - 1 do
                                l := (1. /. n)::!l;
                            done;
                            page_rank mt e !l 0.85 0.15 (List.length a);;

(* Función para comparar páginas a partir de su valor de pagerank. 
   compara_pagina : pagina -> pagina -> bool*)
let compara_pagina p1 p2 = 
    if (p1.pagerank > p2.pagerank) then true else false;;

(* QuickSort para ordenar los resultados. 
   quicksort : ('a -> 'a -> bool) -> 'a list -> 'a list*)
let rec quicksort gt = function
  | [] -> []
  | x::xs ->
      let ys, zs = List.partition (gt x) xs in
      (quicksort gt ys) @ (x :: (quicksort gt zs));;

(* Función que ejecuta el algoritmo y regresa la lista de páginas con el valor
   de PageRank actualizado.
   ejecuta : string -> pagina list*)
let ejecuta archivo =
    let paginas = genera_paginas archivo in
        let a = inicia paginas in
            for i = 0 to (List.length paginas) -1 do
                (List.nth paginas i).pagerank <- (List.nth a i)
            done;
    List.rev (quicksort (compara_pagina) paginas);;

(* Función principal para interacción con el usuario. 
   main : string -> string list *)
let main archivo = 
    let paginas = ejecuta archivo in
        print_string "Introduce una palabra: ";
        let palabra = read_line () in
            let lista = ref [] in
                for i = 0 to (List.length paginas) - 1 do
                    if (List.mem palabra (List.nth paginas i).texto) then
                        lista := (List.nth paginas i).url::!lista;
                done;
            !lista;;