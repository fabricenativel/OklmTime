(* policy + passwd*)
type policy_pwd = {mini :  int; maxi : int; car : char; pwd : string;};;

let day_number = 2;;
let filename = "inputday"^ string_of_int day_number ^ ".txt";;

let rec compte_occ caractere chaine =
  if chaine="" then 0 else
  let reste_chaine = String.sub chaine 1 ((String.length chaine)-1) in
  let premier_caractere = chaine.[0] in
  if premier_caractere=caractere then 1+(compte_occ caractere (reste_chaine)) else compte_occ caractere reste_chaine ;;

let  is_valide item =
  let occ = compte_occ item.car item.pwd in
  if (occ>=item.mini && occ<=item.maxi) then true else false;;

let is_valide2 item =
  let pos1 = item.pwd.[item.mini-1]=item.car && item.pwd.[item.maxi-1]<>item.car in
  let pos2 = item.pwd.[item.mini-1]<>item.car && item.pwd.[item.maxi-1]=item.car in 
  if ((not pos1) && pos2) || (pos1 && (not pos2)) then true else false;;

let make_item min max caractere str =
   {mini = min; maxi=max; car = caractere; pwd = str};;


let reader  = Scanf.Scanning.from_file filename 

let rec parse_input acc =
  try
  let item = Scanf.bscanf reader "%d-%d %c: %s\n" make_item in
  if is_valide2 item then parse_input (acc+1) else parse_input acc
  with End_of_file  -> acc

let() = print_int (parse_input 0); print_newline();;


