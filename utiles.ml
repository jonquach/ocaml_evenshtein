(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2017 ---------------------- *)
(* Fonctions utiles                                                           *)
(* -------------------------------------------------------------------------- *)
module Utiles = struct

  module L = List
  module S = String
  module H = Hashtbl

  let (%) f g = function x -> f (g x)
  let id x = x 

  (* Fonction permettant la lecture de toutes les lignes d'un fichier *)
  let read_lines_file file =
    let ic = open_in file in
    let liste = ref [] in
    let _ = 
      try
        while true do
          let str = input_line ic in liste := str :: (!liste)
        done
      with End_of_file ->
        close_in ic
    in
    L.rev (!liste)

  (* Fonctions utiles pour la manipulation des tables de confusion *)
  let print_matrix ?(n_total=30) ?(n_en_plus=4) m =
    let t = Array.make n_total ' ' in
    begin
      t.(0) <- '#'; t.(1) <- ' '; t.(2) <- '-'; t.(3) <- '\'';
      for i = n_en_plus to (n_total-1) do t.(i) <- Char.chr (i + 93) done;
      Printf.printf "\n\t";
      for i = 0 to (n_total-1) do Printf.printf "%c\t" t.(i) done;
      for i = 0 to (n_total-1) do 
        Printf.printf "\n";
        Printf.printf "%c\t" t.(i);
        for j = 0 to (n_total-1) do 
            Printf.printf "%d\t" m.(i).(j)
        done
      done
    end

  let c_fromInt i = match i with
    | 0 -> '#'
	  | 1 -> ' '
	  | 2 -> '-'
	  | 3 -> '\''
	  | _ -> Char.chr (i + 93) 

  let pos_diff_zero ?(n_total=30) m =
    let l = ref [] in
      for i = 0 to (n_total-1) do 
        for j = 0 to (n_total-1) do 
          if m.(i).(j) <> 0 then l := (c_fromInt i, c_fromInt j, m.(i).(j))::(!l)
        done
      done;
      !l

  (* Fonction utile pour afficher les éléments d'une table de hachage 
     ou récupérer ces éléments sous forme d'une liste de paires
  *) 
  let print_h ?(sep=" : ") fk fv h = 
    H.iter (fun k v -> print_endline ((fk k) ^ sep ^ (fv v))) h

  let ht_to_list f ht =
    H.fold (fun k v acc -> ((f (k,v))::acc)) ht [];;


  (* Fonctions pour la prise en compte du contexte des caractères saisis
     par un utilisateur: majuscules; déliminateurs, etc.    
  *)
  let explode = function 
    | "" -> []
    | s -> 
  	  let rec loop acc = function
    	| 0 -> s.[0] :: acc
    	| x -> loop (s.[x] :: acc) (x - 1)
  in
    loop [] (String.length s - 1)

  let is_uppercase c =
    let i = Char.code c in
    i >= 65 && i <= 90;;

  let is_lowercase c =
    let i = Char.code c in
    i >= 97 && i <= 122;;

  let all_upper w =
    L.for_all is_uppercase (explode w);;

  type case_type = Is_upper | Is_lower | All_upper

  let case w = 
    let n = S.length w in
    if n = 0 || is_lowercase (w.[0]) then
      Is_lower
    else if all_upper w && n > 1 then 
      All_upper
    else if is_uppercase (w.[0]) then
      Is_upper
    else 
      Is_lower

  let to_case c w = 
    if w = "" then "" else
      match c with 
      | Is_upper -> S.capitalize w
      | Is_lower -> w
      | All_upper -> S.uppercase w

  (* Autres fonctions utiles *)
  let top comp liste = match liste with
      | [] -> raise (Failure "Liste vide")
      | [e] -> e
      | e::r -> L.fold_left comp e r

  let explode_to_str w =
    let n = S.length w in
    let rec aux i acc =
      if i >= 0 then aux (i-1) ((S.sub w i 1)::acc) else acc
    in
    aux (n-1) []

end


