(* Programación Declarativa
   Proyecto 2 - PageRank
   Manuel Soto Romero <manu@ciencias.unam.mx> *)

(* Función que toma el nombre de un archivo y devuelve una lista de las líneas
   asociadas al mismo.
   val leer_archivo : string -> string list = <fun> *)
let leer_archivo archivo =
    let file = open_in archivo in
        let lista = ref [] in
            try
                while true do
                    let linea = input_line file in
                        lista := linea :: !lista
                done; List.rev !lista
            with End_of_file ->
                close_in file; List.rev !lista