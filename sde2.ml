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