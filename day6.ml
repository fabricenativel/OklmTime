let day_number = 6;;
let filename = "inputday"^ string_of_int day_number ^ ".txt";;

let separe chaine =
  chaine.[0], String.sub chaine 1 (String.length chaine - 1)

let rec fusion c1 c2 =
  if c2="" then c1 else
    let tete,queue = separe c2 in
    let temp = fusion c1 queue in
    if String.contains  c1 tete then temp  else ((String.make 1 tete) ^ temp)

let rec intersection c1 c2 =
  if c2="" then "" else
    let tete,queue = separe c2 in
    let temp = intersection c1 queue in
    if (String.contains  c1 tete && (not (String.contains temp tete))) then (String.make 1 tete)^temp  else temp


let get_input filename =
  let reader = open_in filename in
  let rec read_all ()=
    try 
      let line = input_line reader in 
      if line="" then line ^ "!" ^ read_all () else line ^ "\n" ^ read_all ();  
    with End_of_file ->
      close_in reader; ""
    in String.split_on_char '!' (read_all ());;

let sep_rep groupe =
  String.split_on_char '\n' groupe;;

let rec compte_rep grp acc =
  match grp with 
  | [] -> String.length acc
  | h::t -> compte_rep t (fusion h acc);;

  let rec compte_rep2 grp acc =
    match grp with 
    | [] -> String.length acc
    | [x] -> String.length acc
    | h::t -> compte_rep2 t (intersection h acc);;

let rec compte_lgroup grp_list =
  match grp_list with
  | [] -> 0
  | h::t -> compte_rep (sep_rep h) "" + compte_lgroup t;;

  let rec compte_lgroup2 grp_list =
    match grp_list with
    | [] -> 0
    | h::t -> let nb = compte_rep2 (sep_rep h) "abcdefghijklmnopqrstuvwxyz" in (nb + compte_lgroup2 t);;

let () =
  let listgrp = get_input filename in
  print_int (compte_lgroup listgrp);
  print_newline();
  print_int (compte_lgroup2 listgrp);
  print_newline()
  ;;

