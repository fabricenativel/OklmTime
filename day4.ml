let day_number = 4;;
let filename = "inputday"^ string_of_int day_number ^ ".txt";;

let required = ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"];;

(* Récupérer les passeports sous la forme d'une liste de chaines de caractères*)
let get_pp filename =
  let reader = open_in filename in
  let rec read_all ()=
    try 
      let line = input_line reader in 
      if line="" then line ^ "!" ^ read_all () else line ^ "\n" ^ read_all ();  
    with End_of_file ->
      close_in reader; ""
    in String.split_on_char '!' (read_all ());;

let rec est_dans s1 s2 acc =
  if String.length s1 > String.length s2 then -1
  else
    if (String.starts_with ~prefix:s1 s2) then acc 
    else (est_dans s1 (String.sub s2 1 ((String.length s2)-1)) (acc+1));;

let rec all_char_in chaine cars =
  match chaine with 
  | "" -> true
  | _ -> String.contains cars chaine.[0] && all_char_in (String.sub chaine 1 ((String.length chaine) - 1)) cars;;

  let rec get_value chaine cle =
    let deb = est_dans cle chaine 0 in
    if deb = -1 then (Printf.printf "Motif %s Non trouvé dans la chaine" cle; "") else 
      (
      let ideb = (deb+(String.length cle)) in
      let str_deb = (String.sub chaine ideb (String.length chaine - ideb)) in
      let res = Scanf.sscanf str_deb  ":%s"  (fun x->x) in
      res);;

let valide_value cle valeur =
  match cle with
  | "byr" -> let vint = int_of_string valeur in vint>=1920 && vint<=2002
  | "iyr" -> let vint = int_of_string valeur in vint>=2010 && vint<=2020
  | "eyr" -> let vint = int_of_string valeur in vint>=2020 && vint<=2030
  | "hgt" -> let vint = int_of_string (String.sub valeur 0 ((String.length valeur)-2)) in
              if String.ends_with valeur ~suffix:"cm" then vint>=150 && vint<=193 else vint>=59 && vint<=76
  | "hcl" -> valeur.[0]='#' && String.length valeur = 7 && all_char_in (String.sub valeur 1 ((String.length valeur)-1)) "0123456789abcdef"
  | "ecl" -> valeur = "amb" || valeur = "blu" || valeur = "brn" || valeur = "gry" || valeur = "grn" || valeur = "hzl" || valeur = "oth"
  | "pid" -> String.length valeur = 9 && all_char_in valeur "0123456789"
  | _ -> failwith "La clé n'existe pas";;

let rec est_valide pp values =
  match values with
  | [] -> true
  | h::t -> ((est_dans h pp 0) <> -1) && (valide_value h (get_value pp h)) && (est_valide pp t);;



let solve = 
  let pl = get_pp filename in
  let rec compte_valide pl values =
    match pl with
    | [] -> 0
    | h::t -> if (est_valide h values) then 1 + compte_valide t values else compte_valide t values
  in
  compte_valide pl required;;


let () = Printf.printf "Nombre de passeports valides = %d \n" solve;; 