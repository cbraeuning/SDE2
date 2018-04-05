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
let get_table_values_cell = function
	([i;j],table) -> List.nth (List.nth table (j-1)) (i-1)
	| (_) -> [];;

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

let rec cell_products_rec = function
	([cell1;cell2]) -> 
		if cell1 == [] then
			[]
		else
			List.append (cell_products_helper((List.hd cell1), cell2)) (cell_products_rec([(List.tl cell1);cell2]))
	| (_) -> [];;

let cell_products = function 
	([cell1;cell2]) -> cell_products_rec ([cell1;cell2])
	| (_) -> [];;

(**
Prototype: form_row1_cell(element,productions)
Input(s): tuple of single terminal element, productions list
Returned Value: corresponding cell in first row of CYK table
Side Effects: none
Signature: val form_row1_cell : ’a * ’a list list -> ’a list = <fun>
Notes: Forms row 1 cells of CYK table as a special case.
*)
let rec form_row1_cell_rec (element,productions) =
	if productions == [] || element == "" then
		[]
	else if (String.get (List.nth (List.nth productions 0) 1) 0) == (String.get element 0) then
		List.append ([List.nth (List.nth productions 0) 0]) (form_row1_cell_rec (element, (List.tl productions)))
	else
		form_row1_cell_rec (element, (List.tl productions));;

let form_row1_cell (element,productions) = form_row1_cell_rec (element,productions);;



