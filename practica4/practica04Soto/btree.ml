(* Arboles binarios con informacion en los nodos, 
   pensando en arboles binarios de busqueda *)
   
type 'a bin_tree = 
    Empty 
  | Node of 'a bin_tree * 'a * 'a bin_tree ;;

 
(* Generar una lista a partir de un arbol *)
let rec list_of_tree = function 
    Empty -> []
  | Node (lt, r, rt) -> (list_of_tree lt) @ (r :: (list_of_tree rt)) ;;

(* Insertar un elemento en un arbol *)
let rec insert x = function
    Empty -> Node (Empty, x, Empty)
  | Node (lt, r, rt) -> if x<r 
			then Node (insert x lt, r, rt) 
			else Node (lt, r, insert x rt) ;;

(* Generar un arbol a partir de una lista *)
let rec tree_of_list = function
    [] -> Empty
  | h::t -> insert h (tree_of_list t) ;;
  
(* Ordenar una lista es equivalente a insertar los elementos en un arbol binario 
   de busqueda y luego obtener una lista del arbol *)
let sort l = list_of_tree (tree_of_list l);;

let rec tree_height t = match t with 
    Empty -> -1
  | Node (Empty, r, Empty) -> 0
  | Node (lt, r, rt) -> 1 + max (tree_height lt) (tree_height rt) ;;


(* Función que obtiene el tamaño de un árbol binario de búsqueda. *)
let rec size a =
  match a with
  | Empty -> 0
  | Node(i,e,d) -> 1 + size i + size d

let rec member n a =
  match a with
  | Empty -> false
  | Node(i,e,d) -> 
    if e = n
    then
      true 
    else
      (member n i) || (member n d)