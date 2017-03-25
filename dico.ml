(* -------------------------------------------------------------------------- *)
(* ----------------------- TP1 - IFT-3000 - Hiver 2017 ---------------------- *)
(* -------------------------------------------------------------------------- *)
(* -- Dictionnaire arborescent ---------------------- ----------------------- *)
(* -------------------------------------------------------------------------- *)
  
module Dico : sig
  type arbre

  val vide : unit -> arbre
  val ajouter : arbre -> string -> unit
  val liste_mots : arbre -> string list
  val creer : string list -> arbre
  val de_fichier : string -> arbre
  val supprimer : arbre -> string -> unit
  val supprimer_mots : arbre -> string list -> unit
  val membre : arbre -> string -> bool
  val liste_par_prefixe : arbre -> string -> string list
  val show : arbre -> unit
  val ratio : arbre -> float
  val print_ratio : arbre -> unit
end =
struct

  (* Fonctions utiles: transformation de string en char list et vice-versa    *)
  (****************************************************************************) 
  module Utiles = struct
    (* explode : string -> char list *)
    let explode = function 
      | "" -> []
      | s -> 
    let rec loop acc = function
      | 0 -> s.[0] :: acc
      | x -> loop (s.[x] :: acc) (x - 1)
    in
      loop [] (String.length s - 1)

    (* implode : char list -> string *)
    let rec implode = function
      | [] -> ""
      | x::xs -> (String.make 1 x) ^ (implode xs)  

    (* read_lines_file : string -> string list *)
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
      List.rev (!liste)
  end

  (****************************************************************************) 
  (* Implantation                                                             *) 
  (****************************************************************************) 
  open List
  open Utiles
    
  (* Principale structure de données utilisée par le dictionnaire arbo.       *)
  (****************************************************************************) 
  type arbre =  Noeud of bool ref  * (char * arbre) list ref
    
  (* Fonction retournant un arbre vide                                        *)
  let vide() = Noeud(ref false,ref[])

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : creer_branche : char list -> arbre                      *)
  (* @Description   : retourne un arbre correspondant à une branche formant 
                      un mots                                                 *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : l'arbre retourné correspond à une branche et est correct*)
  let rec creer_branche lc = match lc with
    | [] -> Noeud (ref true,ref []) 
    | x::r -> Noeud (ref false, ref [(x,creer_branche r)])

  	    
  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : parcours : arbre -> char list -> 
                                 char list * char list * arbre                *)
  (* @Description   : Parcourt un arbre à partir d'une liste de caractères.   
                      Retourne: 1- la liste des caractères que l'on n'a pu  
                      rejoindre (n'existent pas dans le chemin tracée par la
                      liste de caractères donnée en entrée. 2- la liste de  
                      caractères parcourus (visités). 3- et le sous-arbre   
                      atteint à la fin du parcours.                           *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : l'arbre retourné est un sous-arbre de l'arbre en entrée *)
  let parcours a lc = 
    let rec aux (Noeud(b,l) as a) lc lc' = match lc,!l with
      | [],_ -> (lc,lc',a)
      | _::_,[] -> (lc,lc',a)
      | c::r,_ -> 
      	( try 
      	    let a' = assoc c !l in aux a' r (lc'@[c]) 
      	  with 
      	    | Not_found -> (lc,lc',a)
      	)
    in
    aux a lc []

    
  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : nbr_arcs : arbre -> int                                 *)
  (* @Description   : Retourne nombre arcs dans arbre                         *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : le nombre retourné est correct                          *)
  let rec nbr_arcs a = match a with
    | Noeud(_,l) when l = ref [] -> 0
    | Noeud(_,l) -> 
      (length !l) + fold_right (+) (map (fun (_,a) -> nbr_arcs a) !l) 0


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : haut_limite : arbre -> int -> arbre                     *)
  (* @Description   : À partir d'un arbre, retourne la partir de l'arbre 
                      limitée à une hauteur n                                 *)
  (* @Precondition  : n >= 0                                                  *)
  (* @Postcondition : l'arbre retourné est un sous-arbre de celui en argument *)
  let rec haut_limite a n = match a,n with
    | Noeud(b,_),_ when n <= 0 -> Noeud(b,ref [])
    | Noeud(_,l),_ when l = ref [] -> a
    | Noeud(b,l),_ -> Noeud(b,ref (map (fun (c,a) -> c,haut_limite a (n-1)) !l))

      
  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : ajouter : arbre -> string -> unit                       *)
  (* @Description   : Ajoute un mot à l'arbre                                 *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : effet de bord: l'arbre passé en argument comprend 
                      désormais le mot                                        *)
  let ajouter a mot = match parcours a (explode mot) with
   | [],_,Noeud(b,_) -> b := true
   | c::r,_,Noeud(_,l) -> l := [(c,creer_branche r)]@(!l)
      

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : liste_mots : arbre -> string list                       *)
  (* @Description   : Retourne la liste des mots de l'arbre                   *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : la liste retournée est correcte                         *)
  let liste_mots a = 
    let rec aux (Noeud(b,l)) lres lc = match !l with
      | [] -> 
        ( match !b with
          | false -> lres
          | true -> lres @ [implode lc]
        )
      | (c,a)::r ->
        ( match !b with
          | false -> aux a lres (lc@[c])
          | true -> aux a (lres @ [implode lc]) (lc@[c]) 
        )
        @ 
        aux (Noeud(ref false,ref r)) [] lc 
    in
    aux a [] []
    

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : creer : string list -> arbre                            *)
  (* @Description   : Crée un arbre à partir d'une liste de mots              *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : l'arbre retourné est correct                            *)
  let creer l = 
    let a = vide() in
    let _ = iter (ajouter a) l in
    a       


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : de_fichier : string -> arbre                            *)
  (* @Description   : Crée un arbre à partir des mots présents dans un fichier*)
  (* @Precondition  : Chaque ligne du fichier comprend un mot                 *)
  (* @Postcondition : l'arbre retourné est correct                            *)
  let de_fichier f =
    creer (read_lines_file f)


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : supprimer : arbre -> string -> unit                     *)
  (* @Description   : Supprime un mot d'un arbre                              *)
  (* @Precondition  : Mot existe das l'arbre; autrement exception Not_found   *)
  (* @Postcondition : effet de bord: l'arbre passé en argument ne comprend 
                      désormais plus le mot                                   *)
  let supprimer a mot =            
    let rec parcours (Noeud(b,l')) lc ((c,l) as paire) = match lc,!l' with
    	| [],[] -> l := remove_assoc c !l          (* supprime une branche *)
    	| [],_ -> 
  	    ( match !b with
  	      | true -> b := false                   (* suppprime le mot *)
  	      | false -> raise Not_found             (* mot inexistant *)
  	    )
    	| c::r,[] -> raise Not_found               (* mot inexistant *)
    	| c::r,_ -> 
  	    let a' = assoc c !l' in                  (* Not_found si c inexistant *)
  	    let paire' = if !b || (length !l') > 1 then (c,l') else paire in
               (* soit j'ai un true, soit mon arbre est limite à un sous-arbre*)
  	      parcours a' r paire' 
    in
    let lc = explode mot in
    let Noeud(b,l) = a in
    	match !l,lc with
    	  | _,[] -> if !b then b := false else raise Not_found
    	  | [],_ -> raise Not_found
    	  | ((c,_)::_),_ -> parcours a lc (c,l) 
    

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : supprimer_mots : arbre -> string list -> unit           *)
  (* @Description   : Supprime une liste de mots d'un arbre                   *)
  (* @Precondition  : Mots existent das l'arbre; autrement exception Not_found*)
  (* @Postcondition : effet de bord: l'arbre passé en argument ne comprend 
                      désormais plus les mots                                 *)
  let supprimer_mots a l = 
    iter (supprimer a) l
  		  

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : membre: arbre -> string -> bool                         *)
  (* @Description   : Teste si un mot est dans un arbre                       *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : vrai si mot existe; faux, sinon                         *)
  let membre a mot = 
    match parcours a (explode mot) with
  	| ([],_,Noeud(b,_)) -> !b <> false
  	| _ -> false


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : sous_arbre: ?h_limit:int option -> arbre -> string -> 
                                  arbre                                       *)
  (* @Description   : À partir d'un mot, retourne le sous-arbre qui suit les  
                      caractères formant ce mot; si h_limit=None, retourne 
                      tout le sous-arbre; sinon, Some(n), retourne le sous-
                      arbre limité à une hauteur = n                          *)
  (* @Precondition  : Mot existe dans l'arbre; autrement exception Not_found  *)
  (* @Postcondition : l'arbre retourné est un sous-arbre de celui en argument *)
  let sous_arbre ?(h_limit = None) a mot = 
    match parcours a (explode mot) with
      | [],_,a -> 
        begin
          match h_limit with
            | None -> a
            | Some n -> haut_limite a n
        end
      | _ -> raise Not_found
            

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : liste_par_prefixe: arbre -> string -> string list       *)
  (* @Description   : Étant donné un arbre, et un prefixe (mot), retourne tous
  	                  les mots de l'arbre ayant ce prefixe; la liste des mots
  	                  retournée est triée, par ordre croissant, selon la taille
  	                  de ces mots                                             *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : la liste retournée est correcte                         *)
  let liste_par_prefixe a m =
    let compare m1 m2 = 
      compare (String.length m1) (String.length m2)
    in
    sort compare 
         (map (fun x -> m ^ x) (liste_mots (sous_arbre a m)))
         

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : show: arbre -> unit                                     *)
  (* @Description   : Affiche un arbre sous forme graphique, grâce à l'outil
                      dotty (http://www.graphviz.org/)                        *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : aucune                                                  *)
  let show (Noeud(b,_) as a) = 
    let format x y c = 
      Printf.sprintf "%s -- %s [ label = \"%c\" ];\n" x y c in
    let init n s =
      let s' = ref "" in 
      for i = 1 to n do s' := !s' ^ s ^ (string_of_int i) ^ "; " done; 
      !s'
        in
        let node b cf ct = match !b with
          | true -> "t" ^ (string_of_int !ct)
          | false -> "f" ^ (string_of_int !cf)
        in
        let rec show_tree (Noeud(b,l)) cf ct =
          let node_b = node b cf ct in 
      fold_left (fun (s,cf,ct) (c,(Noeud(b',_) as a)) ->
        let _ = if !b' then incr ct else incr cf in
        let node_b' = node b' cf ct in
        let (s',cf',ct') = show_tree a cf ct in
          (s ^ (format node_b node_b' c) ^ s', cf', ct')
      ) ("",cf,ct) !l
        in
        let (cf,ct) = if !b then (ref 0,ref 1) else (ref 1,ref 0) in
        let (body,cf,ct) = show_tree a cf ct in 
        let graphviz = 
          "graph G {\n" ^
          "node [shape=none]; {node [label=\"false\"] " ^ 
      (init !cf "f") ^ 
            "}\nnode [shape=none]; {node [label=\"true\" fontcolor=blue] " ^ 
      (init !ct "t") ^ 
      "}\n\n" ^ body ^ "\n}\n"
        in
        let file = "graph.gv" in
        let fout = open_out file in
          output_string fout graphviz;
          close_out fout;
          ignore (Sys.command ("xdot " ^ file));
          ()    


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : ratio: arbre -> float                                   *)
  (* @Description   : retourne le ratio entre le nombre total de caractères
                      présent dans tous les mots du dictionnaire et le nombre 
                      de caractères (arcs) effectivement présent dans l'arbre *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : aucune                                                  *)
  let ratio a = 
    let n1 = fold_right (fun x acc -> (String.length x) + acc) (liste_mots a) 0 
    and n2 = nbr_arcs a in
    abs_float((1.0 -. (float(n2) /. float(n1)))) *. 100.0


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : print_ratio: arbre -> unit                              *)
  (* @Description   : Affiche le ratio de (gain en) stockage                  *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : aucune                                                  *)
  let print_ratio a = 
    print_endline 
      ("Ratio de stockage : " ^ string_of_int(int_of_float(ratio a)) ^ "%")

end