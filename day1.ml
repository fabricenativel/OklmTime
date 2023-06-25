let filename = "inputday1.txt"

(* Lire les lignes du fichier et les stockes sous forme d'entier dans une liste *)
let input =
  let reader = open_in filename in
  let rec read_all acc =
  try 
    let line = input_line reader in 
    read_all (int_of_string line :: acc)
  with End_of_file ->
    close_in reader; acc
in read_all []


let fuel x =
  x / 3 - 2

  (* calcul du fuel requis pour chaque input puis somme*)
let solve =
  List.fold_left (+) 0 (List.map fuel input);;

Printf.printf "Réponse : %i\n" solve