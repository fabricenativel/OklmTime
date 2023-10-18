let filename = "inputday5.txt"

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

let getpm op =
    let opcode = op mod 100 in
        let pm1 = (op/100) mod 10 in
          let pm2 = (op/1000) mod 10 in
            let pm3 = (op/10000) mod 10 in
            (pm3,pm2,pm1,opcode)

let rec run_programm pgm pos =
  match getpm pgm.(pos) with
  |  _,  0, 0, 1 ->  pgm.(pgm.(pos+3)) <- pgm.(pgm.(pos+2)) + pgm.(pgm.(pos+1));  run_programm pgm (pos+4)
  |  _, 0, 1, 1 ->  pgm.(pgm.(pos+3)) <- pgm.(pgm.(pos+2)) + pgm.(pos+1);  run_programm pgm (pos+4)
  |  _, 1, 0, 1 ->  pgm.(pgm.(pos+3)) <- pgm.(pos+2) + pgm.(pgm.(pos+1));  run_programm pgm (pos+4)
  |  _, 1, 1, 1 ->  pgm.(pgm.(pos+3)) <- pgm.(pos+2) + pgm.(pos+1);  run_programm pgm (pos+4)
  |  _,  0, 0, 2 ->  pgm.(pgm.(pos+3)) <- pgm.(pgm.(pos+2)) * pgm.(pgm.(pos+1));  run_programm pgm (pos+4)
  |  _, 0, 1, 2 ->  pgm.(pgm.(pos+3)) <- pgm.(pgm.(pos+2)) * pgm.(pos+1);  run_programm pgm (pos+4)
  |  _, 1, 0, 2 ->  pgm.(pgm.(pos+3)) <- pgm.(pos+2) * pgm.(pgm.(pos+1));  run_programm pgm (pos+4)
  |  _, 1, 1, 2 ->  pgm.(pgm.(pos+3)) <- pgm.(pos+2) * pgm.(pos+1);  run_programm pgm (pos+4)
  |  _, _, 0, 3 ->  pgm.(pgm.(pos+1)) <- 1; run_programm pgm (pos+2)
  |  _, _, 1, 3 ->  pgm.(pos+1) <- 5; run_programm pgm (pos+2)
  |  _, _, 0, 4 ->  print_int pgm.(pgm.(pos+1)) ; run_programm pgm (pos+2)
  |  _, _, 1, 4 ->  print_int pgm.(pos+1) ; run_programm pgm (pos+2)
  |  _, 0, 0, 5 -> if pgm.(pgm.(pos+1))<>0 then run_programm pgm (pgm.(pos+2)) else run_programm pgm (pos+3)
  |  _, 0, 1, 5 -> if pgm.(pos+1)<>0 then run_programm pgm (pgm.(pos+2)) else run_programm pgm (pos+3)
  |  _, 1, 0, 5 -> if pgm.(pgm.(pos+1))<>0 then run_programm pgm (pos+2) else run_programm pgm (pos+3)
  |  _, 1, 1, 5 -> if pgm.(pos+1)<>0 then run_programm pgm (pos+2) else run_programm pgm (pos+3)
  |  _, 0, 0, 6 -> if pgm.(pgm.(pos+1))=0 then run_programm pgm (pgm.(pos+2)) else run_programm pgm (pos+3)
  |  _, 0, 1, 6 -> if pgm.(pos+1)=0 then run_programm pgm (pgm.(pos+2)) else run_programm pgm (pos+3)
  |  _, 1, 0, 6 -> if pgm.(pgm.(pos+1))=0 then run_programm pgm (pos+2) else run_programm pgm (pos+3)
  |  _, 1, 1, 6 -> if pgm.(pos+1)=0 then run_programm pgm (pos+2) else run_programm pgm (pos+3)
  |  _, 0, 1, 7 -> if pgm.(pos+1)=0 then run_programm pgm (pgm.(pos+2)) else run_programm pgm (pos+3)
  |  _, 1, 0, 7 -> if pgm.(pgm.(pos+1))=0 then run_programm pgm (pos+2) else run_programm pgm (pos+3)
  |  _, 0, 0, 7 -> if pgm.(pgm.(pos+1))=0 then run_programm pgm (pgm.(pos+2)) else run_programm pgm (pos+3)
  |  _, 1, 1, 7 -> if pgm.(pos+1)=0 then run_programm pgm (pos+2) else run_programm pgm (pos+3)
  | _, _, _, 99 -> ()
  | _ -> print_int pgm.(pos); failwith "Bug !" 



let () = 
  let input = get_input in
  run_programm input 0


