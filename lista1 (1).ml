(* zadanie 1 *)
let rec flatten a =  
  if (a = [] ) then [] 
  else List.hd a  @ flatten(List.tl a) ;;

flatten [[1;2;9]; [3;5]; [7]; [9;1]] = [1;2;9;3;5;7;9;1];;
flatten [[5;6]; [1;2;3]] = [5;6;1;2;3] ;;
flatten[["Ala";"ma";"kota"]] = ["Ala";"ma";"kota"];;
flatten [] = [] ;;


(*zadanie 2 *)

let rec count (a, list) = 
  if List.length list = 0 then 0 
  else if List.hd list = a then count (a, List.tl list) + 1
  else count (a, List.tl list) ;;

count('a', ['a'; '1'; 'a'; 'a'; 'a']) = 4;; 
count(34, 19 :: 25 :: 34 :: 69 :: 34 :: []) = 2;;
count(7, [1; 2; 7; 99; 7; 31; 7; 7; 7]) = 5;;
count(0, []) = 0;;
count(0, [1; 2; 99; 31]) = 0;;

(*zadanie 3*)

let rec replicate (a, int) =
  if (int <= 0) then []
  else [a] @ replicate(a, int-1) ;;

replicate("ia", 3) = ["ia"; "ia"; "ia"];;
replicate(7, 0) = [] ;;
replicate(1023, 3) = [1023; 1023; 1023] ;;


(*zadanie 4*)

let rec sqrList lista =
  if lista = [] then lista
  else [List.hd lista * List.hd lista] @ sqrList(List.tl lista) ;;

sqrList [1; 2; 3; -4] = [1; 4; 9; 16] ;;
sqrList [] = [] ;;
sqrList [-7; 7; 1; 0; 5] = [49; 49; 1; 0; 25] ;;


(*zadanie 5 *)

let rec odwrocListe lista = 
  if List.length lista = 0 then lista
  else odwrocListe (List.tl lista) @ [List.hd lista] ;;

let palindrome lista = 
  lista = odwrocListe lista ;;

palindrome ['a'; 'l'; 'a'] = true ;;
palindrome [] = true;;
palindrome [1; 4; 8; 9; 7; 9; 8; 4; 1] = true;;
palindrome [1; 2; 3; 1] = false;;
palindrome [1; 2; 2; 1] = true;;


(* zadanie 6 *)

let rec listLength lista = 
  if lista = [] then 0
  else listLength(List.tl lista) + 1 ;;

listLength[11; 22; 33; 44; 55; 66; 77] = 7;;
listLength[-3; -3; -3; -3] = 4;;
listLength[] = 0;;
listLength['a'; 'a'; 'w'] = 3;;






  

