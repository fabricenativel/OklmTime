let day_number = 7;;
let filename = "inputday"^ string_of_int day_number ^ ".txt";;

type contenu = {ccolor : string; number : int}

type bag = {bcolor: string; contenu : contenu list}

let affiche_c cont =
    Printf.printf "Couleur : %s, nombre ; %d ;" cont.ccolor cont.number;;

let rec affiche_cl cl = 
  match cl with
  | [] -> ()
  | h::t -> affiche_c h; affiche_cl t;;

let  affiche_b b =
  Printf.printf "%s [" b.bcolor;
  affiche_cl b.contenu;
  Printf.printf "]\n";;

let get_input filename =
  let reader = open_in filename in
  let rec read_all ()=
    try 
      let line = input_line reader in 
      line ^ "\n" ^ read_all ();
    with End_of_file ->
      close_in reader; ""
    in String.split_on_char '\n' (read_all ());;

let rec make_content cstr =
  match cstr with
  | [] -> []
  | h::t -> let lr = Scanf.Scanning.from_string (String.trim h) in 
  let tnumber,tcolor = Scanf.bscanf lr "%d %s bag" (fun x y -> x,y) in
  {ccolor=tcolor; number=tnumber}::(make_content t)

let make_bag str =
  let reader = Scanf.Scanning.from_string str in
  let bcolor, bcontent = Scanf.bscanf reader "%s bags contain %s" (fun x y->x,y) in
  {bcolor = bcolor; contenu = (make_content (String.split_on_char ',' bcontent))}

let rec make_bag_list slist =
  match slist with
  | [""] -> []
  | [] -> []
  | h::t -> (make_bag h)::(make_bag_list t)

let ()=
  let rs = make_bag_list (get_input filename) in
  affiche_b (List.hd rs);
