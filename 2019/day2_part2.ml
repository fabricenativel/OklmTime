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

(* execute le programme avec les données de start jusqu'à trouver la valeur target*)
let rec solve target start =
  if start>9999 then failwith "Bug !" else
    begin
  let pgm = get_input in
  let noun = start/100 in
  let verb = start mod 100 in 
      let cpgm = Array.copy pgm in    
      cpgm.(1) <- noun;
      cpgm.(2) <- verb;
      let res = run_programm cpgm 0 in
      if res=target then start else solve target (start + 1)
    end

let () = Printf.printf "Réponse : %i\n" (solve 19690720 0)

