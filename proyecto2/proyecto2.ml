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
    pagerank : float;
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


(* Función que construye una matriz de nxn y la inicializa con el valor v. *)
let crea_matriz n v =
    Array.make n (Array.make n (float_of_int v));;

(* Función que construye la gráfica de adyacencias *)
let construye_grafia paginas matriz =
    for i = 0 to (List.length paginas) - 1 do
        for j = 0 to (List.length (List.nth paginas i).links) - 1 do
            let n = (List.length (List.nth paginas i).links) in
                matriz.(i).((List.nth (List.nth paginas i).links) j) <- 1. /. (float_of_int n);
        done;
    done;
    matriz;;