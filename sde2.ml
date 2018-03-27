(* Collin Braeuning - cbraeun 
%% SDE1	- CYK Parse Tables *)

(* all productions (in CNF) here let is used for illustration *)
(* S -> AB *)
let prod1 = ["S"; "AB"];; (* Note: NOT ["S";"A";"B"] *)
(* A -> a *)
let prod2 = ["A"; "a"];;
(* B -> b *)
let prod3 = ["B"; "b"];;
(* C -> b *)
let prod4 = ["C"; "b"];;
(* list of all producitons *)
let productions = [prod1;prod2;prod3;prod4];;
(* string to parse *)
let astring = ["a"; "a"; "b"; "b"];;

(**
Prototype: get_table_values_cell([i;j],table)
Input(s): tuple of ([<column>;<row>], table)
Returned Value: cell with string values
Side Effects: none
Signature: val get_table_values_cell : int list * ’a list list -> ’a = <fun>
*)
let get_table_values_cell ([i;j],table) =
	List.nth (List.nth table (j-1)) (i-1);; 

(**
Prototype: cell_products [cell1;cell2]
Input(s): list containing 2 cells
Returned Value: resultant list of strings
Side Effects: none
Signature: val cell_products : string list list -> string list = <fun>
*)
let rec cell_products_helper (item, cell2) =
	if cell2 == [] then
		[]
	else
		List.append [String.concat "" (List.append [item] [List.hd cell2])] (cell_products_helper(item, (List.tl cell2)));;

let rec cell_products_rec ([cell1;cell2]) = 
	if cell1 == [] then
		[]
	else
		List.append (cell_products_helper((List.hd cell1), cell2)) (cell_products([(List.tl cell1);cell2]));;

let cell_products ([cell1;cell2]) = cell_products_rec ([cell1;cell2]);;


