
let filename = "inputday1.txt";;

(* Lire les lignes du fichier et les stockes sous forme d'entier dans une liste *)
let input =
  let reader = open_in filename in
  let rec read_all acc =
  try 
    let line = input_line reader in 
    read_all (int_of_string line :: acc)
  with End_of_file ->
    close_in reader; acc
in read_all [];;

let rec solve data target = 
  match data with
  | [] -> -1
  | h::t -> if List.mem (target-h) t then h*(target-h) else (solve t target);;

let rec solve2 data target =
  match data with
  | [] -> failwith "Pas de solution"
  | h::t -> let temp =  solve t (target-h) in
    if temp <> -1 then temp*h else solve2 t target

  
let rep = solve2 input 2020;;

let () = print_string "Réponse : "; print_int rep; print_endline " ";;


