
let day_number = 3;;
let filename = "inputday"^ string_of_int day_number ^ ".txt";;
let pentes = [(1,1); (3,1); (5,1); (7,1); (1,2)];;

(* Récupère le nombre de lignes et de colonnes de l'entrée*)
let rec get_size filename  =
  let reader = open_in filename in
  (* lire une ligne pour avoir le nombre de colonne *)
  let col = String.length (input_line reader) in
  (* lire récursivement les autres lignes pour avoir leur nombre *)
  let rec compte_ligne reader =
  try
    let _ = input_line reader in 
    1 + compte_ligne reader
  with
    End_of_file -> close_in reader; 1 in
  let lig = compte_ligne reader in
  lig, col;;



(* parse les données pour créer un tableau *)
let parse_data filename =
  let nb_lig, nb_col = get_size filename in
  Printf.printf "Dimension du tableau = %dx%d\n" nb_lig nb_col;
  let data = Array.make_matrix nb_lig nb_col '.' in
  let reader = open_in filename in
  let rec lire_caractere lig col = 
    let car = input_char reader in
    if (lig = nb_lig-1 && col=nb_col) then data else
      if col == nb_col then lire_caractere (lig+1) 0
      else
        (data.(lig).(col)<- car; 
        lire_caractere lig (col+1))
  in lire_caractere 0 0;;

let rec compte_arbre data pente posx posy =
if posy >= Array.length data then 0 else
let nposx = (posx + fst pente) mod Array.length data.(0) in
if data.(posy).(posx)='#' then 1 + compte_arbre data pente nposx (posy+ snd pente) else  compte_arbre data pente nposx (posy+ snd pente)
let solve =
let data = parse_data filename in
let rec compte_all lpentes =
  match lpentes with
  | [] -> 1
  | h::t -> compte_arbre data h 0 0 * (compte_all t) 
in 
let res = compte_all pentes in
Printf.printf "Nombre d'arbres rencontrés : %d\n" res;

