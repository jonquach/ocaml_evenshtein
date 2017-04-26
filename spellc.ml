(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2017 ---------------------- *)
(* -------------------------------------------------------------------------- *)
(* Matricule étudiant: .........                                              *)
(* Matricule étudiant: .........                                              *)
(* Matricule étudiant: .........                                              *)
(* -------------------------------------------------------------------------- *)
(* -- PRINCIPALE FICHIER DU TP ---------------------- ----------------------- *)
(* -------------------------------------------------------------------------- *)
  
#load "unix.cma";;
#load "str.cma";;

#use "dico.ml";;
#use "utiles.ml";;

(* Module prinicpal -------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)
module Spellc = struct

  open Utiles
  open Dico

  module L = List
  module S = String
  module H = Hashtbl

  exception Non_Implante of string

  let k = 2
  let bow = '#'
  let bol = "<s>"
  let eol = "</s>"

  (* on accepte [a-zA-Z]+ et l'apostrophe                                     *)
  let special_c = "[\000-\038\040-\064\091-\096\123-\255]+"
    
  let corpus = "corpus.txt"
  let misspell = "misspell.txt"
  
  type operation =
    | SUB of char * char    (* char source par char cible             *)
    | DEL of char * char    (* char source et celui qui le précède    *)
    | INS of char * char    (* char à insérer et celui qui le précède *)
    | TRANS of char * char  (* char courant et suivant à transposer   *)

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : string -> string -> int * operation list                *)
  (* @Description   : version modifiée de Wiki:                               *)
  (*            https://fr.wikipedia.org/wiki/Distance_de_Damerau-Levenshtein *) 
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : entier retourné positif ou nul                          *)
  (* ------------------------------------------------------------------------ *)
  let dist s t =
    let m = String.length s
    and n = String.length t in
    let d = Array.make_matrix (m+1) (n+1) 0
    and bt = Array.make_matrix (m+1) (n+1) None in 
    let rec f mat i j acc =
      if i <0 || j < 0 then acc else
      match (mat.(i).(j)) with
      | Some op -> (match op with
        | DEL (x, y) -> (f mat (i-1) (j) ((DEL(x, y))::acc))
        | INS (x, y) -> f mat (i) (j-1) ((INS(x, y))::acc)
        | SUB (x, y) -> f mat (i-1) (j-1) ((SUB(x, y))::acc)
        | TRANS (x, y) -> f mat (i-2) (j-2) ((TRANS(x, y))::acc))
      | None -> f mat (i-1) (j-1) acc
    in
    for i = 1 to m do
      d.(i).(0) <- i ;
      bt.(i).(0) <- 
        if i == 1 
        then Some (DEL(s.[i-1], '#')) 
        else Some (DEL(s.[i-1], s.[i-2]))
    done;
    for j = 1 to n do
      d.(0).(j) <- j ;
      bt.(0).(j) <- Some (INS (t.[j-1], '#'))
    done;
    for j = 1 to n do
      for i = 1 to m do
        if s.[i-1] = t.[j-1] then
          d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
        else
          let del, ins, sub = ((d.(i-1).(j) + 1), 
            (d.(i).(j-1) + 1), (d.(i-1).(j-1) + 1)) in
          if del <= ins && del <= sub then
            (d.(i).(j) <- del; bt.(i).(j) <- 
              if i == 1 
              then Some (DEL(s.[i-1], '#')) 
            else Some (DEL(s.[i-1], s.[i-2])))
          else 
            if ins <= del && ins <= sub then
              (d.(i).(j) <- ins; bt.(i).(j) <- Some (INS (t.[j-1], s.[i-1])))
            else 
              if sub <= ins && sub <= del then
                (d.(i).(j) <-sub; bt.(i).(j) <- Some (SUB (s.[i-1], t.[j-1])))
          ;
          if ((i > 1) && (j > 1) && (s.[i-1] = t.[j-2]) && (s.[i-2] = t.[j-1])) 
          then
            let trans = (d.(i-2).(j-2)+1) in
            if trans < d.(i).(j) then
            (d.(i).(j) <- trans; bt.(i).(j) <- Some (TRANS (s.[i-2], s.[i-1])))
      done;
    done;
    d.(m).(n), (f bt m n [])


  (* --  À IMPLANTER/COMPLÉTER (5 PTS) ------ Fonction liste_words ---------- *)
  (* @Fonction      : string -> string list list                              *)
  (* @Description   : retourne une liste de listes de mots d'un fichier donné;*)
  (*                  chacune de ces listes correspond à une ligne du fichier.*) 
  (* @Precondition  : fichier existe                                          *)
  (* @Postcondition : ordre des lignes traitées du fichier n'importe pas.     *)
  (* ------------------------------------------------------------------------ *)
  let split_line line =
    Str.split (Str.regexp special_c) line

  let liste_words file =let liste = read_lines_file file in
    let (l1,l2) = List.partition (fun x-> x == []) (
    List.map split_line (List.map String.lowercase liste)) in l2


    
  (* --  À IMPLANTER/COMPLÉTER (8 PTS) ------ Fonction wwpf_frequence ------- *)
  (* @Fonction      : string -> (string, int) H.t *                           *)
  (*                            (string * string, int) H.t * int * int        *)
  (* @Description   : retourne la fréquence de mots et de paires de mots,     *) 
  (*                  ainsi que le nombre total de mots et de mots différents.*)
  (* @Precondition  : fichier existe                                          *)
  (* @Postcondition : les tables et les nombres retournés sont corrects.      *)
  (* ------------------------------------------------------------------------ *)
  let rec remove_dups lst = match lst with
    | [] -> []
    | h::t -> h::(remove_dups (List.filter (fun x -> x <> h) t))

  let word_freq l =
    let hash = Hashtbl.create 250 in
    L.iter (fun key -> if Hashtbl.mem hash key then Hashtbl.replace hash key ((Hashtbl.find hash key) + 1) else Hashtbl.add hash key 1) l;
    hash

  let add_bol lw =
    L.map (fun x -> ["<s>"] @ x) lw

  let add_eol lw =
    L.map (fun x -> ["<s>"] @ x @ ["</s>"]) lw

  let helper_fun llw = let rec aux_liste l acc = match l with
    | x::r when r != [] -> aux_liste r (acc@[(x,List.hd r)])
    | _-> acc in 
    let reponse = List.map (fun l -> aux_liste l []) llw in 
    List.flatten reponse


  let wwpf_frequence file =
    let lw = L.flatten(liste_words file) in
    let nlw = liste_words file in
    let wf = word_freq(L.flatten(add_bol nlw)) in 
    let wpf = word_freq(L.flatten(add_eol nlw)) in
    let _N = ref 0 in 
    let _V = ref 0 in 
    _N := L.length lw;
    _V := L.length(remove_dups lw);
    (* Compléter par votre code *)
    (* raise (Non_Implante "«wwpf_frequence» à compléter") *)
    wf, wpf, !_V, !_N

  let wf, wpf, _V, _N = wwpf_frequence corpus
  

  (* --  À IMPLANTER/COMPLÉTER (5 PTS) ------ Fonction liste_words' --------- *)
  (* @Fonction      : string -> (string * string list) list                   *)
  (* @Description   : retourne une liste de paires associant des mots à des   *)
  (*                  listes de mots.                                         *)
  (* @Precondition  : fichier existe                                          *)
  (* @Postcondition : le résultat retourné est correct.                       *)
  (* ------------------------------------------------------------------------ *)
  let split_line' line =
    Str.split (Str.regexp "->") line

  let split_line'' line =
    Str.split (Str.regexp ", ") line

  let liste_words' file = let l1 = read_lines_file file in
   let (l2,l3) = List.partition (fun x-> (String.length x) == 0) l1 in
   let l4 = List.map String.lowercase l3 in
      List.map (fun l-> match l with
                          | [a;b] -> (a,split_line'' b) 
                          | _-> failwith "Error:cas impossible!")
               (List.map split_line' l4)


  (* --  À IMPLANTER/COMPLÉTER (5 PTS) ------ Fonction word_pair_list ------- *)
  (* @Fonction      : string -> (string * string) list                        *)
  (* @Description   : retourne une liste de paires associant des mots bien    *)
  (*                  orthographiés à des mots mal orthographiés.             *)
  (* @Precondition  : fichier existe                                          *)
  (* @Postcondition : le résultat retourné est correct.                       *)
  (* ------------------------------------------------------------------------ *)
  let word_pair_list file = let l1 = liste_words' file in
    List.concat (List.map (fun (a,b) -> List.map (fun x-> (x,a)) b) l1)

    
  (* --  À IMPLANTER/COMPLÉTER (7 PTS) ------ Fonction gen_matrix ----------- *)
  (* @Fonction      : string -> int array array * int array array *           *)
  (*                            int array array * int array array             *)
  (* @Description   : construit 4 matrices de confusion (voir énoncé).        *)
  (* @Precondition  : fichier existe                                          *)
  (* @Postcondition : le résultat retourné est correct.                       *)
  (* ------------------------------------------------------------------------ *)
  let n_en_plus = 4
  let n_total = 26 + n_en_plus

  let i_fromChar c = match c with
  | '#' -> 0
  | ' ' -> 1
  | '-' -> 2
  | '\'' -> 3
  | _ -> (Char.code c) - 97 + n_en_plus
    
  let gen_matrix file =
    let subm = Array.make_matrix (n_total) (n_total) 0 in
    let delm = Array.make_matrix (n_total) (n_total) 0 in
    let insm = Array.make_matrix (n_total) (n_total) 0 in
    let tram = Array.make_matrix (n_total) (n_total) 0 in
    let l1 = word_pair_list file in
    let list_op = List.map (fun (a,b)->(dist a b)) l1 in
      List.iter (fun liste-> let (_,l) = liste in let rec aux l = match l with
                  | x::r-> (match x with
                            | DEL (c1, c2) -> delm.(i_fromChar c1).(i_fromChar c2)<-(delm.(i_fromChar c1).(i_fromChar c2)+1);aux r
                            | INS (c1, c2) -> insm.(i_fromChar c1).(i_fromChar c2)<-(delm.(i_fromChar c1).(i_fromChar c2)+1);aux r
                            | SUB (c1, c2) -> subm.(i_fromChar c1).(i_fromChar c2)<-(delm.(i_fromChar c1).(i_fromChar c2)+1);aux r
                            | TRANS (c1, c2) -> tram.(i_fromChar c1).(i_fromChar c2)<-(delm.(i_fromChar c1).(i_fromChar c2)+1);aux r)
                  | []-> () in aux l) list_op;
    subm, delm, insm, tram

  let subm, delm, insm, tram = gen_matrix misspell


  (* --  À IMPLANTER/COMPLÉTER (5 PTS) ------ Fonction c_cc_gen ------------- *)
  (* @Fonction      : string -> string list                                   *)
  (* @Description   : à partir d'un mot, retourne la liste des caractères et  *)
  (*                  des séquences de paires de caractères.                  *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : le résultat retourné est correct.                       *)
  (*                  Si le mot est vide, retourne la liste vide.             *)
  (* ------------------------------------------------------------------------ *)
  let c_cc_gen w = match w with
    | "" -> []
    | _ -> let liste_1 = ["#"]@List.map (String.make 1) (Utiles.explode w) in
          let rec aux l acc = match l with
            | e::r when r != [] -> aux r (acc@[e^(List.hd r)])
            | _->acc in
          liste_1@(aux liste_1 [])


    

  (* --  À IMPLANTER/COMPLÉTER (7 PTS) ------ Fonction gen_table ------------ *)
  (* @Fonction      : string -> (string, int) H.t                             *)
  (* @Description   : retourne la fréquence de caractères et de séquences de  *)
  (*                  2 caractères présents dans un fichier particulier.      *)
  (* @Precondition  : fichier existe.                                         *)
  (* @Postcondition : le résultat retourné est correct.                       *)
  (* ------------------------------------------------------------------------ *)
  let gen_table file =
    let cpcf = H.create 1000 in 
    (* Compléter par votre code *)
    (* raise (Non_Implante "«gen_table» à compléter") *)
    cpcf

  let cpcf = gen_table misspell
  let _K = H.length cpcf


  (* --  À IMPLANTER/COMPLÉTER (8 PTS) ------ Fonction p_fault -------------- *)
  (* @Fonction      : operation -> float                                      *)
  (* @Description   : équation (3) de l'énoncé.                               *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : le résultat retourné est compris entre 0. et 1.         *)
  (* ------------------------------------------------------------------------ *)
  let p_fault op = 
    (* Remplacer la ligne suivante par votre code *)
    (* raise (Non_Implante "«p_fault» à compléter") *)


  (* --  À IMPLANTER/COMPLÉTER (4 PTS) ------ Fonction prob_xw -------------- *)
  (* @Fonction      : operation list -> float                                 *)
  (* @Description   : équation (4) de l'énoncé.                               *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : le résultat retourné est compris entre 0. et 1.         *)
  (* ------------------------------------------------------------------------ *)
  let prob_xw ops =
    (* Remplacer la ligne suivante par votre code *)
    (* raise (Non_Implante "«prob_xw» à compléter") *)


  (* --  À IMPLANTER/COMPLÉTER (8 PTS) ------ Fonction prob_uwv ------------- *)
  (* @Fonction      : ?u:string -> ?v:string -> string -> float               *)
  (* @Description   : section 1.2.2, page 4, de l'énoncé                      *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : le résultat retourné est compris entre 0. et 1.         *)
  (* ------------------------------------------------------------------------ *)
 let prob_uwv ?(u = "") ?(v = "") w = 
    (* Remplacer la ligne suivante par votre code *)
    raise (Non_Implante "«prob_uwv» à compléter")


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : ?u:string -> ?v:string -> string -> operation list      *)
  (*                  -> float                                                *)
  (* @Description   : équation (4) de l'énoncé.                               *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : le résultat retourné est compris entre 0. et 1.         *)
  (* ------------------------------------------------------------------------ *)
  let prob_xw_uwv ?(u = "") ?(v = "") w ops =
    (prob_xw ops) *. (prob_uwv ~u:u ~v:v w)


  (* --  À IMPLANTER/COMPLÉTER (8 PTS) ------ Fonction find_candidates ------ *)
  (* @Fonction      : ?k:int -> string -> (string * operation list) list      *)
  (* @Description   : retourne candidats potentiels par rapport à un mot mal  *)
  (*                  orthographié (étape #2 de section de l'énoncé)          *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : les mots retournés appartiennent à la table wf.         *)
  (* ------------------------------------------------------------------------ *)
  let find_candidates ?(k = 2) x =
    (* Remplacer la ligne suivante par votre code *)
    raise (Non_Implante "«find_candidates» à compléter")


  (* --  À IMPLANTER/COMPLÉTER (5 PTS) ------ Fonction best_candidate  ------ *)
  (* @Fonction      : ?u:string -> ?v:string -> string ->                     *)
  (*                  (string * operation list) list -> string                *)
  (* @Description   : retourne le meilleur candidat parmi les candidats       *)
  (*                  identifiés (étape #3 de section de l'énoncé)            *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : le mot retourné fait partie des candidats.              *)
  (*                  Si la liste cand est vide, retourne x.                  *)
  (* ------------------------------------------------------------------------ *)
  let best_candidate ?(u = "") ?(v = "") x cand =
    (* Remplacer la ligne suivante par votre code *)
    raise (Non_Implante "«best_candidate» à compléter")


  (* --  À IMPLANTER/COMPLÉTER (5 PTS) ------ Fonction tri_gramme ----------- *)
  (* @Fonction      : string list -> (string * string * string) list          *)
  (* @Description   : à partir d'une liste de mots, retourne la liste des     *)
  (*                  des séquences de 3 mots (prise en compte de bol/eol.    *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : les mots dans la liste résultant proviennent de lwords. *)
  (* ------------------------------------------------------------------------ *)
  let tri_gramme lwords =
    (* Remplacer la ligne suivante par votre code *)
    raise (Non_Implante "«tri_gramme» à compléter")


  (* --  À IMPLANTER/COMPLÉTER (10 PTS) ------ Fonction revise_lwords ------- *)
  (* @Fonction      : ?k:int -> string list -> string list                    *)
  (* @Description   : principale fonction de correction automatique d'une     *)
  (*                  liste de mots, formant une phrase, avec prise en compte *) 
  (*                  du contexte.                                            *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : les mots retournés sont dans le dictionnaire.           *)
  (* ------------------------------------------------------------------------ *)
  let revise_lwords ?(k = 2) lwords =
    (* Remplacer la ligne suivante par votre code *)
    raise (Non_Implante "«revise_lwords» à compléter")


  (* -- revise_line --------------------------------------------------------- *)
  (* @Fonction      : ?k:int -> string -> string                              *)
  (* @Description   : corrige automatiquement une phrase donnée.              *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : la phrase retournée comprend des mots du dictionnaire.  *)
  (* ------------------------------------------------------------------------ *)
  let split_input_line line =
    let l = Str.full_split (Str.regexp special_c) line in
    let lwords = 
      L.fold_left 
        (fun acc s -> match s with Str.Delim d -> acc | Str.Text t -> (t::acc))
        [] l in
    l, L.rev lwords
  
  let desplit_line lsep lwords = 
    let rec aux lsep lwords acc = match lsep, lwords with
      | [],[] -> acc
      | [],[s] -> acc ^ s 
      | [Str.Delim s],[] -> acc ^ s
      | x::r, x'::r' -> 
        begin
          match x with
            | Str.Delim s -> aux r lwords (acc ^ s) 
            | Str.Text _ -> aux r r' (acc ^ x')
        end
      | _ -> failwith "deslit_line impossible!"
    in
    aux lsep lwords ""

  let revise_line ?(k = 2) line =
    let lsplit, lwords = split_input_line line in
    let case_list = L.map case lwords in
    let lwords' = revise_lwords ~k:k lwords in
    let lwords'' = L.map2 (fun c w -> to_case c w) case_list lwords' in
    desplit_line lsplit lwords'' 


  (* --  À IMPLANTER/COMPLÉTER (10 PTS) ------ Fonction find_cand_aux ------- *)
  (* @Fonction      : ?k:int -> ?u:string -> string -> (string * float) list  *)
  (* @Description   : principale fonction de completion automatique d'un      *)
  (*                  prefixe saisi par l'utilisateur (avec prise en compte du*)
  (*                  mot précédemment saisi.                                 *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : la liste des mots retournés sont dans le dictionnaire,  *)
  (*                  et les réels qui leur sont associés sont entre [0,1].   *)
  (* ------------------------------------------------------------------------ *)
  let dict = creer (ht_to_list fst wf)

  let find_cand_aux ?(k = 2) ?(u = bol) pref =
    (* Remplacer la ligne suivante par votre code *)
    raise (Non_Implante "«find_cand_aux» à compléter")

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : ?k:int -> ?u:string -> string -> string list            *)
  (* @Description   : fonction utilisant find_cand_aux; effectue des pre et   *)
  (*                  post traitements ; la fonction find_cand est la         *)
  (*                  principale fonction utilisée par gui2.ml.               *)
  (* @Precondition  : aucune.                                                 *)
  (* @Postcondition : la liste des mots retournés sont dans le dictionnaire.  *)
  (* ------------------------------------------------------------------------ *)
  let find_cand ?(k = 2) ?(u = bol) pref =
    let m = S.length pref in
    let u = if u = "" then bol else S.lowercase u in
    let case_t = if u = bol && m = 0 then Is_upper else case pref in 
    let pref = S.lowercase pref in
	  let candidates = find_cand_aux ~k:2 ~u:u pref in
    L.map (fun (w,_) -> if w = eol then "." else to_case case_t w) 
    	  candidates

end

