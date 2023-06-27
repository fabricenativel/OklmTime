let filename = "inputday2.txt"

(* Lire la ligne d'entrée, split et créer un array d'entiers*)
let get_input =
  let reader = open_in filename in
    let line = input_line reader in 
    Array.of_list (List.map int_of_string (String.split_on_char ',' line))

    (* Fonctions d'affichage pour débugger !*)
let disp_int n =
  print_int(n); print_char(' ')

let affiche pgm =
  Array.iter disp_int  pgm;
  print_endline(" ")
  

let rec run_programm pgm pos =
  match pgm.(pos) with
  | 1 ->  pgm.(pgm.(pos+3)) <- pgm.(pgm.(pos+2)) + pgm.(pgm.(pos+1));  run_programm pgm (pos+4)
  | 2 ->  pgm.(pgm.(pos+3)) <- pgm.(pgm.(pos+2)) * pgm.(pgm.(pos+1));  run_programm pgm (pos+4)
  | 99 -> pgm.(0)
  | _ -> failwith "Bug !" 

let () = 
let input = get_input in
input.(1) <- 12;
input.(2) <- 2;
let res = run_programm input 0 in
Printf.printf "Réponse : %i\n" res

