let filename = "inputday3.txt"
let size = 10000

type node = {lig : int; col : int;}

type wire = node list

(* dir = {'R' : (1,0), 'L' : (-1,0), }*)

let norme1 n=
abs(n.lig) + abs(n.col)

let comp n1 n2 =
  if n1 = n2 then 0
  else if norme1 n1 < norme1 n2 then -1 
  else if norme1 n1 > norme1 n2 then  1
  else if n1.lig < n2.lig || (n1.lig=n2.lig && n1.col<n2.col) then -1
  else 1


let print_node n = Printf.printf "(%d,%d) " n.lig n.col

let print_wire = List.iter (print_node)

let rec make_node sx sy dir len =
  if len = 0 then [] else 
    match dir with 
    |'R' ->  {lig  = sy; col = (sx+1)} :: make_node (sx+1) sy dir (len-1)
    |'L' ->  {lig  = sy; col = (sx-1)} :: make_node (sx-1) sy dir (len-1)
    |'U' ->  {lig  = (sy+1); col = sx} :: make_node sx (sy+1) dir (len-1)
    |'D' ->  {lig  = (sy-1); col = sx} :: make_node sx (sy-1) dir (len-1)
    | _ -> failwith "Bug dans les données"


let rec get_wire data sx sy=
  match data with 
  |[] -> []
  | h :: t -> 
    let dir = String.get h 0 in 
    let len = int_of_string (String.sub h 1 ((String.length h)-1)) in 
      match dir with
      |'R' -> (make_node sx sy dir len)@(get_wire t (sx+len) sy)
      |'L' -> (make_node sx sy dir len)@(get_wire t (sx-len) sy)
      |'U' -> (make_node sx sy dir len)@(get_wire t sx (sy+len))
      |'D' -> (make_node sx sy dir len)@(get_wire t sx (sy-len))
      | _ -> failwith "Bug dans les données"


let rec intersection w1 w2 =
  match w1 with
  | [] -> []
  | h::t -> if List.mem h w2 then h::(intersection t w2) else intersection t w2


let rec find_intersection wire1 wire2 =
  match wire1, wire2 with
  | [],_ -> failwith "Bug"
  | _,[] -> failwith "Bug"
  | h1::t1, h2::t2 when h1=h2 -> h1
  | h1::t1, h2::t2  -> if comp h1 h2 = -1 then find_intersection t1 wire2 else find_intersection wire1 t2

(* parse l'entrée pour récupérer les deux matrices*)
let get_input = 
  let reader = open_in filename in
    let line = input_line reader in 
    let data1 = String.split_on_char ',' line in
    let wire1 = get_wire data1 0 0 in
    let line = input_line reader in 
    let data2 = String.split_on_char ',' line in
    let wire2 = get_wire data2 0 0 in
    Printf.printf "Taille wire1 %d \n" (List.length wire1);
    Printf.printf "Taille wire2 %d \n" (List.length wire2);
    print_wire wire1;
    print_newline ();
    let wire1_trie = List.fast_sort comp wire1 in 
    let wire2_trie = List.fast_sort comp wire2 in
    print_wire wire1_trie;
    print_newline ();
    let solution = find_intersection wire1_trie wire2_trie in 
    print_node solution

let () = get_input 

    


