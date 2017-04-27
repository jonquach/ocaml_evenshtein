(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2017 ---------------------- *)
(* Tests #2 des fonctions à implanter; GROS FICHIERS CORPUS ET MISSPELL2       *)
(* -------------------------------------------------------------------------- *)

(* Fonctions utiles pour les tests                                            *) 
(******************************************************************************) 
(* l1 included in l2 *)
let ( <+ ) l1 l2 = 
  List.fold_right (fun x b -> (List.exists (fun y -> x=y) l2) && b) l1 true;;

(* l1 equal_set l2 *)
let ( <=> ) l1 l2 = (l1 <+ l2) && (l2 <+ l1);;


(* DEBUT TESTS                                                                *) 
(******************************************************************************) 

#use "spellc.ml";;
open Utiles;;
open Spellc;;

(* Test de la fonction liste_words *)
let t_liste_words = List.length (liste_words "corpus2.txt") = 15743;;

(* Test de la fonction wwpf_frequence *)
let t_wwpf_frequence = 
  _V = 22166 && _N = 362337 && 
  Hashtbl.length wf = 22167 && 
  Hashtbl.length wpf = 175608 &&
  Hashtbl.find wf "realise" = 10 &&
  Hashtbl.find wpf ("to","realise") = 3;;

(* Test de la fonction liste_words' *)
let t_liste_words' = List.length (liste_words' "misspell2.txt") = 4269;;

(* Test de la fonction word_pair_list *)
let t_word_pair_list = List.length (word_pair_list "misspell2.txt") = 4504;;

(* Test de la fonction gen_matrix *)
let t_gen_matrix = 
  (List.map (List.length % pos_diff_zero) [subm; delm; insm; tram]) = 
  [151; 250; 259; 157];;

(* Test de la fonction c_cc_gen *)
let t_c_cc_gen = List.for_all2 (fun l l' -> l <=> l')
      (List.map c_cc_gen ["";"a";"ab";"abc"])
      [[]; ["#"; "a"; "#a"]; ["#"; "a"; "b"; "#a"; "ab"];
      ["#"; "a"; "b"; "c"; "#a"; "ab"; "bc"]];;

(* Test de la fonction gen_table *)
let t_gen_table = 
  let l = ht_to_list id cpcf in
  Hashtbl.length cpcf = 516 &&
  (List.filter (fun (s,_) -> String.length s = 1) l) <=>
  [("o", 2398); ("x", 119); ("l", 2087); ("k", 99); ("z", 66); ("n", 3171);
     ("s", 2829); (" ", 33); ("f", 438); ("c", 2043); ("t", 2887); ("b", 590);
     ("v", 484); ("d", 1401); ("q", 73); ("m", 1240); ("i", 3825); ("-", 4);
     ("'", 14); ("r", 3006); ("y", 791); ("h", 803); ("g", 947); ("e", 5007);
     ("j", 31); ("p", 1250); ("#", 4504); ("w", 184); ("u", 1400); ("a", 3277)] 
    &&
  (List.filter (fun (_,n) -> n >= 500) l) <=>
  [("es", 593); ("nt", 588); ("en", 719); ("o", 2398); ("l", 2087);
     ("#a", 538); ("n", 3171); ("s", 2829); ("ti", 678); ("c", 2043);
     ("ed", 544); ("t", 2887); ("b", 590); ("d", 1401); ("er", 619);
     ("m", 1240); ("#c", 504); ("re", 668); ("i", 3825); ("te", 587);
     ("at", 554); ("r", 3006); ("y", 791); ("h", 803); ("g", 947); ("e", 5007);
     ("p", 1250); ("in", 776); ("#", 4504); ("u", 1400); ("on", 677);
     ("a", 3277)];;

(* Test de la fonction p_fault *)
let t81 = p_fault (INS('a','b')) = 0.0018083182640144665;;
let t82 = p_fault (INS('b','a')) = 0.00079093066174532034;;
let t83 = p_fault (INS('e','d')) = 0.0067814293166405838;;
let t84 = p_fault (TRANS('h','a')) = 0.015576323987538941;;
let t_p_fault = t81 && t82 && t83 && t84;;

(* Test de la fonction prob_xw *)
let t91 = prob_xw [INS('e','d'); TRANS('h','a')] = 0.00010562974013458853;;
let t92 = prob_xw [INS('a','b'); INS('b','a')] = 1.4302543612031109e-006;;
let t_prob_xw = t91 && t92;;

(* Test de la fonction prob_uwv *)
let t101 = (List.map prob_uwv ["back";"hope";"to"]) =
      [0.00053835730800539923; 0.00017945243600179972; 
       0.023586291914497418];;
let t102 = (List.map (fun (x,y) -> prob_uwv ~u:x y) 
           ["hope","to";"realize","to";"i","hope";
            "<s>","hope";"<s>","i"]) =
      [0.0011693802284789062; 4.5114138771090859e-005; 
       0.0001781340458695168; 5.2757920282782451e-005; 
       0.0029808224959772087];;
let t103 = (List.map (fun (x,y) -> prob_uwv x ~v:y) 
           ["future","</s>";"dream","</s>";"hope","to";"to","be"]) =
      [2.407808612362816e-007; 1.8767040577676044e-009; 
       2.0984813061288084e-007; 0.000436475530722274];;
let t104 = (List.map (fun (x,y,z) -> prob_uwv ~u:x y ~v:z) 
           ["i","hope","to";"the","future","</s>";
            "to","realize","my";"to","realize","that";
            "the","future","</s>";"<s>","i","have";
            "<s>","i","hope"]) =
      [2.0830643125876752e-007; 9.3884169646801225e-007; 
       1.4443919693632216e-009; 1.4443919693632216e-009; 
       9.3884169646801225e-007; 1.3274649280682292e-006;
       5.3098597122729167e-007];;
let t_prob_uwv = t101 && t102 && t103 && t104;;

(* Test de la fonction find_candidates *)
let t_find_candidates = List.for_all2 (fun l l' -> l <=> l')
        (List.map find_candidates ["academc";"accesion"])
        [[("academic", [DEL ('i', 'm')]);
          ("academia", [SUB ('i', 'c'); DEL ('a', 'i')]);
          ("academics", [DEL ('i', 'm'); DEL ('s', 'c')]);
          ("academy", [SUB ('y', 'c')])];
       [("accession", [DEL ('s', 'e')]);
          ("accretion", [DEL ('r', 'c'); SUB ('t', 's')]);
          ("ascension", [SUB ('s', 'c'); DEL ('n', 'e')]);
          ("occasion", [SUB ('o', 'a'); SUB ('a', 'e')])]];;

(* Test de la fonction best_candidate *)
let t121 = (List.map (fun c -> best_candidate c (find_candidates c)) 
           ["hve";"dram";"futuer";"te";"bck";"ope"]) =
       ["have"; "dream"; "future"; "the"; "back"; "open"];;
let t122 = (List.map (fun (u,c) -> best_candidate ~u:u c (find_candidates c)) 
           ["i","hve";"in","te"]) =
       ["have"; "the"];;
let t123 = (List.map (fun (u,c,v) -> best_candidate ~u:u c ~v:v 
                   (find_candidates c)) 
           ["i","hve","to";"in","te","future"]) =
       ["have"; "the"];;
let t_best_candidate = t121 && t122 && t123;;

(* Test de la fonction tri_gramme *)
let t_tri_gramme = List.map tri_gramme 
              [[];["m"];["m1"; "m2"];["m1"; "m2"; "m3"]] =
                  [[]; [("<s>", "m", "</s>")]; 
                   [("<s>", "m1", "m2"); ("m1", "m2", "</s>")];
                 [("<s>", "m1", "m2"); ("m1", "m2", "m3"); 
                  ("m2", "m3", "</s>")]];;

(* Test de la fonction revise_lwords *)
(* let t_revise_lwords = List.map (revise_lwords % split_line) 
                 ["i hve a dream";"i hpe to be";
                "i hpe to understand the meaning";
                "to reaize my fuure"] =
        [["i"; "have"; "a"; "dream"]; ["i"; "hope"; "to"; "be"];
      ["i"; "hope"; "to"; "understand"; "the"; "meaning"];
      ["to"; "realise"; "my"; "future"]];;

(* Test de la fonction find_cand_aux *)
let t151 = List.map find_cand_aux ["accommodat";"achievig";"guarantid"] =
      [[("accommodate", 2.6378960141391225e-005)];
       [("achieve", 6.0880785103715134e-010);
          ("achieved", 4.3850362040664921e-010)];
       [("guarantee", 2.283029441389317e-010)]];;
let t_find_cand_aux = t151;; *)

(* RÉSULTATS                                                                  *) 
(******************************************************************************) 
let test_all = 
  t_liste_words && t_wwpf_frequence && t_liste_words' && t_word_pair_list && 
  t_gen_matrix && t_c_cc_gen && t_gen_table && t_p_fault && t_prob_xw && 
  t_prob_uwv && t_find_candidates && t_best_candidate && t_tri_gramme;; (* && 
  t_revise_lwords && t_find_cand_aux;; *)

Printf.printf "\nResults\n";
Printf.printf "-------\n";
Printf.printf "\tliste_mots : %b\n" t_liste_words;
Printf.printf "\twwpf_frequence : %b\n" t_wwpf_frequence;
Printf.printf "\tliste_words' : %b\n" t_liste_words';
Printf.printf "\tword_pair_list : %b\n" t_word_pair_list;
Printf.printf "\tgen_matrix : %b\n" t_gen_matrix;
Printf.printf "\tc_cc_gen : %b\n" t_c_cc_gen;
Printf.printf "\tgen_table : %b\n" t_gen_table;
Printf.printf "\tp_fault : %b\n" t_p_fault;
Printf.printf "\tprob_xw : %b\n" t_prob_xw;
Printf.printf "\tprob_uwv : %b\n" t_prob_uwv;
Printf.printf "\tfind_candidates : %b\n" t_find_candidates;
Printf.printf "\tbest_candidate : %b\n" t_best_candidate;
Printf.printf "\ttri_gramme : %b\n" t_tri_gramme;
(* Printf.printf "\trevise_lwords : %b\n" t_revise_lwords;
Printf.printf "\tfind_cand_aux : %b\n\n" t_find_cand_aux; *)
Printf.printf "\tTOUT EST OK : %b\n\n" test_all;;

(* FIN TESTS                                                                  *) 
(******************************************************************************) 
