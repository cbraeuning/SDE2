(* Collin Braeuning - cbraeun 
%% SDE1	- CYK Parse Tables *)

(* all productions (in CNF) here let is used for illustration *)
let prod1 = ["S"; "AB"];; (* Note: NOT ["S";"A";"B"] *)
let prod2 = ["A"; "a"];;
let prod3 = ["B"; "b"];;
let prod4 = ["C"; "b"];;
(* list of all producitons *)
let prodb1 = ["s"; "AB"];;
let prodb2 = ["S"; "A"];;
let prodb3 = ["AB"; "CD"];;
let prodb4 = ["s"; "cAB"];;

let productions = [prod1;prod2;prod3;prod4];;
(* string to parse *)
let astring = ["a"; "a"; "b"; "b"];;

let tablebook  =
[[["A"]; ["A"]; ["B"; "C"]; ["B"; "C"]];
[["C"]; ["S"; "A"]; ["S"; "B"; "A"]]; [["C"; "A"]; ["C"; "S"; "A"]];
[["C"; "B"; "S"; "A"]]];;

let tablebook2 =
[[["A"]; ["A"]; ["B"; "C"]; ["B"; "C"]];
[["C"]; ["A"; "S"]; ["B"; "A"; "S"]]; [["C"; "A"]; ["A"; "S"; "C"]];
[["S"; "B"; "C"; "A"]]];;

let tablebook3 =
[[["A"]; ["A"]; ["B"; "C"]; ["B"; "C"]];
[["C"]; ["A"; "G"]; ["B"; "A"; "S"]]; [["C"; "A"]; ["A"; "S"; "C"]];
[["S"; "B"; "C"; "A"]]];;

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

(**
Prototype: equiv(ca, cb)
Inputs: tuple of 2 cells
Returned Value: true or false
Side Effects: none
Signature: val equiv : ’a list * ’a list -> bool = <fun>
*)
let rec is_member_int(a,cb) = 
	if cb == [] then
		false
	else if  a == (List.hd cb) then
		true
	else is_member_int(a, List.tl cb);;

let rec equiv_rec(ca,cb) =
	if (ca == []) then
		true
	else if (is_member_int(List.hd ca, cb) == false) then
		false
	else 
		equiv_rec( List.tl ca, cb);;

let equiv (ca, cb) = 
	if(List.length(ca) != List.length(cb)) then
		false
	else if (ca == [] && cb == []) then
		true
	else equiv_rec(ca, cb);;

(**
Prototype: row_equivalent(rowA,rowB)
Inputs: tuple of 2 rows
Returned Value: true or false
Side Effects: none
Signature: val row_equivalent : ’a list list * ’a list list -> bool = <fun>
**)
let rec is_member_str(a,cb) = 
	if cb == [] then
		false
	else if (String.length(a) == String.length(List.hd cb)) && ((Char.code(String.get a 0)) == (Char.code(String.get (List.hd cb) 0))) then
		true
	else is_member(a, List.tl cb);;
	
let rec equiv_str_rec(ca,cb) =
	if (ca == []) then
		true
	else if (is_member_str(List.hd ca, cb) == false) then
		false
	else 
		equiv_str_rec( List.tl ca, cb);;

let equiv_str (ca, cb) = 
	if(List.length(ca) != List.length(cb)) then
		false
	else if (ca == [] && cb == []) then
		true
	else equiv_str_rec(ca, cb);;

let rec row_equivalent_rec (rowA,rowB) =
	if rowA == [] && rowB == [] then
		true 
	else if equiv_str(List.hd rowA, List.hd rowB) == false then
		false
	else row_equivalent_rec(List.tl rowA, List.tl rowB);;

let row_equivalent (rowA,rowB) =
	if (List.length rowA) != (List.length rowB) then
		false
	else row_equivalent_rec (rowA, rowB);;

(**
Prototype: table_equivalent(tableA,tableB)
Inputs: tuple of 2 tables
Returned Value: true or false
Side Effects: none
Signature: val table_equivalent :
’a list list list * ’a list list list -> bool = <fun>
**)
let rec table_equivalent_rec (tableA,tableB) =
	if (tableA == []) && (tableB == []) then
		true
	else if row_equivalent_rec(List.hd tableA, List.hd tableB) == false then
		false
	else table_equivalent_rec(List.tl tableA, List.tl tableB);;

let table_equivalent (tableA,tableB) = 
	if (List.length tableA) != (List.length tableB) then
		false
	else table_equivalent_rec (tableA,tableB);;

(**
Prototype: valid_production production
Inputs: a list
Returned Value: true or false
Side Effects: none
Signature: val valid_production : string list -> bool = <fun>
Notes: true if production is valid format and CNF
**)
let isUpperCase (a) = 
	if ((Char.code a) < 91) && ((Char.code a) > 64) then 
		true
	else false;;

let isLowerCase (a) = 
	if ((Char.code a) < 123) && ((Char.code a) > 96) then
		true
	else false;;

let valid_production production = 
	if List.length production != 2 then
		false
	else if String.length (List.hd production) != 1 || isLowerCase (String.get (List.hd production) 0) then
		false
	else if (String.length (List.nth production 1) == 1) && (isLowerCase (String.get (List.nth production 1) 0)) then
		true
	else if (String.length (List.nth production 1) == 2) && (isUpperCase (String.get (List.nth production 1) 0)) && (isUpperCase (String.get (List.nth production 1) 1)) then
		true
	else false;;

(**
Prototype: valid_production_list productionList
Inputs: list of productions
Returned Value: true or false
Side Effects: none
Signature: val valid_production_list : string list list -> bool = <fun>
*)
let rec valid_production_list_rec productionList = 
	if productionList == [] then
		true
	else if valid_production (List.hd productionList) == false then
		false
	else valid_production_list_rec (List.tl productionList);;

let valid_production_list productionList = valid_production_list_rec productionList;;
