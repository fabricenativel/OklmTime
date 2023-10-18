let rec exprap x k = match k with
  | 0 -> 1
  | k when k mod 2 = 0 -> let p = exprap x (k/2) in p*p
  | k -> let p = exprap x (k/2) in x*p*p

let chiffre i n =
  if i = 0 then n mod 10
  else (n / (exprap 10 i)) mod 10

let is_ok c =
  let l = List.init 6 (fun i -> chiffre (5-i) c) in
  let rec loop double curr_adja = function
    | [] -> double
    | [x] -> double || curr_adja = 2
    | x::y::t when x <= y -> if x <> y then loop (curr_adja = 2 || double) 1 (y::t)
                                else (* x=y *)
                                    loop double (curr_adja+1) (y::t)
    | _ -> false
  in
  loop false 1 l

let prgm debut fin =
  let rec prgm_loop count i =
    if i > fin then count
    else if is_ok i then prgm_loop (count+1) (i+1)
    else prgm_loop count (i+1)
  in
  prgm_loop 0 debut

let print_bool = function true -> print_endline "true" | false -> print_endline "false"

let () =
  is_ok 112233 |> print_bool;
  is_ok 123444 |> print_bool;
  is_ok 111122 |> print_bool

let () = print_int (prgm 158126 624574)