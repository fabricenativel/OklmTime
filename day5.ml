let day_number = 5;;
let filename = "inputday"^ string_of_int day_number ^ ".txt";;

type intervalle = {vmin : int; vmax: int};;

let range = {vmin=0; vmax=127};;
let col = {vmin = 0; vmax = 7};;

let part inter car =
  let mid = (inter.vmin + inter.vmax)/2 in
  match car with
  | 'F' | 'L' -> {vmin =inter.vmin; vmax=mid }
  | 'B' | 'R' -> {vmin =mid+1 ; vmax=inter.vmax }
  | _ -> failwith "Erreur"

let rec partage inter chaine =
  match chaine with
  | "" -> inter
  | _ -> let new_int = part inter chaine.[0] in partage new_int (String.sub chaine 1 ((String.length chaine)-1))

let affiche inter =
  Printf.printf "[%d;%d]\n" inter.vmin inter.vmax;;

let get_id chaine =
  let chaine1 = String.sub chaine 0 7 in
  let chaine2 = String.sub chaine 7 3 in
  let rnum = (partage range chaine1).vmin in
  let snum = (partage col chaine2).vmin in
  rnum*8 + snum;;

(* Récupérer chaque ligne du fichier dans une liste*)
  let get_data filename =
    let reader = open_in filename in
    let rec read_all ()=
      try 
        let line = input_line reader in 
         line ^"\n"^ read_all ();  
      with End_of_file ->
        close_in reader; ""
      in String.split_on_char '\n' (String.trim (read_all ()));;

let rec find_max enc acc =
  match enc with
  | [] -> acc
  | h::t -> let cv = get_id h in 
    if cv>acc then find_max t cv else find_max t acc;;

let rec discard seen enc =
  match enc with
  | [] -> seen
  | h::t -> let cv = get_id h in 
  seen.(cv) <- true; discard seen t;;

let rec view_false res idx maxidx =
  if idx>maxidx then () else (
  if (not res.(idx)) then Printf.printf "Manquant = %d\n" idx else ();
  view_false res (idx+1) maxidx);;

let () =
let encoded = get_data filename in
let smax = find_max encoded 0 in
Printf.printf "Le siége avec le plus grand ID = %d\n" smax;
let seen = Array.make (smax+1) false in 
let res = discard seen encoded in
view_false res 0 smax;;
