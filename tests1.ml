(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2017 ---------------------- *)
(* Tests #1 des fonctions à implanter; tests présents dans l'énoncé du Tp.    *)
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
let t11 = (liste_words "vide.txt") = [];;
let t12 = (liste_words "corpus.txt") <=>
      [["back"; "to"; "the"; "future"];
       ["i"; "hope"; "to"; "be"; "in"; "the"; "future"];
       ["the"; "objective"; "is"; "to"; "realize"; "my"; "dreams"];
       ["i"; "hope"; "to"; "realize"; "that"; "dream"];
       ["i"; "have"; "a"; "dream"]];;
let t_liste_words = t11 && t12;;

(* Test de la fonction wwpf_frequence *)
let h1,h2,n1,n2 = wwpf_frequence "vide.txt";;
let t21 = (H.length h1 = 0) && (H.length h2 = 0) && n1 = 0 && n2 = 0;;

let h1,h2,n1,n2 = wwpf_frequence "corpus.txt";;
let t22 = (n1 = 17) && (n2 = 28) &&
      (ht_to_list id h1 <=> 
        [("dream", 2); ("be", 1); ("hope", 2); ("dreams", 1); 
         ("to", 4); ("in", 1); ("i", 3); ("future", 2); ("the", 3); 
         ("is", 1); ("have", 1); ("<s>", 5); ("that", 1); ("my", 1); 
         ("back", 1); ("a", 1); ("realize", 2);("objective", 1)]
      ) &&
      (ht_to_list id h2 <=> 
        [(("a", "dream"), 1); (("to", "realize"), 2); 
         (("the", "future"), 2); (("<s>", "the"), 1); 
         (("have", "a"), 1); (("dreams", "</s>"), 1); 
         (("my", "dreams"), 1); (("<s>", "i"), 3); 
         (("realize", "my"), 1); (("dream", "</s>"), 2); 
         (("to", "be"), 1); (("back", "to"), 1); 
         (("to", "the"), 1); (("is", "to"), 1); (("i", "hope"), 2);
       (("realize", "that"), 1); (("<s>", "back"), 1); 
       (("i", "have"), 1); (("that", "dream"), 1); (("in", "the"), 1); 
       (("the", "objective"), 1); (("future", "</s>"), 2); 
       (("objective", "is"), 1); (("hope", "to"), 2); (("be", "in"), 1)]
      );;
let t_wwpf_frequence = t21 && t22;;

(* Test de la fonction liste_words' *)
let t31 = (liste_words' "vide.txt") = [];;
let t32 = (liste_words' "misspell.txt" <=>
       [("comback", ["comeback"]); ("bcak", ["back"]); ("bakc", ["back"]);
      ("backrounds", ["backgrounds"]); ("backgorund", ["background"]);
      ("ackward", ["awkward"; "backward"]); 
      ("relized", ["realised"; "realized"]);
      ("dreasm", ["dreams"]); ("deram", ["dram"; "dream"]);
      ("haev", ["have"; "heave"]); ("ahve", ["have"]); 
      ("ahppen", ["happen"]);
      ("ahev", ["have"])]
       );;
let t_liste_words' = t31 && t32;;

(* Test de la fonction word_pair_list *)
let t41 = (word_pair_list "vide.txt") = [];;
let t42 = (word_pair_list "misspell.txt" <=>
       [("comeback", "comback"); ("back", "bcak"); ("back", "bakc");
      ("backgrounds", "backrounds"); ("background", "backgorund");
      ("awkward", "ackward"); ("backward", "ackward"); 
      ("realised", "relized"); ("realized", "relized"); 
      ("dreams", "dreasm"); ("dram", "deram"); ("dream", "deram"); 
      ("have", "haev"); ("heave", "haev"); ("have", "ahve");  
      ("happen", "ahppen"); ("have", "ahev")]
       );;
let t_word_pair_list = t41 && t42;;

(* Test de la fonction gen_matrix *)
let subm, delm, insm, tram = gen_matrix "vide.txt";;
let t51 = (List.map pos_diff_zero [subm;delm;insm;tram]) = [[];[];[];[]];;

let subm, delm, insm, tram = gen_matrix "misspell.txt";;
let t52 = (List.map pos_diff_zero [subm;delm;insm;tram]) = 
      [ [('z', 's', 1); ('c', 'w', 1)];
        [('v', 'e', 1); ('m', 'e', 1); ('k', 'g', 1); 
         ('e', 'a', 2); ('#', 'b', 1)];
      [('d', 'e', 1)];
      [('v', 'e', 2); ('r', 'o', 1); ('r', 'e', 1); 
       ('m', 's', 1); ('h', 'a', 3);
         ('e', 'a', 1); ('c', 'k', 1); ('a', 'c', 1)]
        ];;
let t_gen_matrix = t51 && t52;;

(* Test de la fonction c_cc_gen *)
let t_c_cc_gen = List.for_all2 (fun l l' -> l <=> l')
      (List.map c_cc_gen ["";"a";"ab";"abc"])
      [[]; ["#"; "a"; "#a"]; ["#"; "a"; "b"; "#a"; "ab"];
      ["#"; "a"; "b"; "c"; "#a"; "ab"; "bc"]];;

(* Test de la fonction gen_table *)
let th = gen_table "vide.txt";;
let t71 = (H.length th = 0) ;;

let th = gen_table "misspell.txt";;
let t72 = ht_to_list id th <=> 
      [ ("co", 1); ("ms", 1); ("ra", 1); ("iz", 1); ("ds", 1); ("me", 1); 
        ("en", 1); ("o", 3); ("av", 4); ("#b", 5); ("aw", 1); ("rd", 2); 
        ("l", 2); ("ap", 1); ("k", 7); ("#a", 1); ("ar", 2); ("z", 1); 
        ("n", 3); ("s", 3); ("al", 2); ("nd", 2); ("kw", 2); ("wa", 2); 
        ("un", 2); ("gr", 2); ("ve", 4); ("ba", 6); ("dr", 3); ("c", 7); 
        ("kg", 2); ("ed", 2); ("am", 3); ("b", 6); ("li", 2); ("v", 4); 
        ("d", 9); ("is", 1); ("m", 4); ("#c", 1); ("re", 4); ("i", 2);      
        ("ac", 6); ("#d", 3); ("ou", 2); ("ze", 1); ("ha", 4); ("r", 9); 
        ("ro", 2); ("pp", 1); ("se", 1); ("h", 5); ("g", 2); ("eb", 1); 
        ("e", 13); ("om", 1); ("pe", 1); ("#r", 2); ("p", 2); ("he", 1); 
        ("ck", 6); ("#", 17); ("wk", 1); ("ea", 5); ("w", 3); ("u", 2); 
        ("#h", 5); ("a", 19)
      ] ;;
let t_gen_table = t71 && t72;;

(* Test de la fonction p_fault *)
let t81 = p_fault (INS('a','b')) = 0.013513513513513514;;
let t82 = p_fault (INS('b','a')) = 0.011494252873563218;;
let t83 = p_fault (INS('e','d')) = 0.025974025974025976;;
let t84 = p_fault (TRANS('h','a')) = 0.055555555555555552;;
let t_p_fault = t81 && t82 && t83 && t84;;

(* Test de la fonction prob_xw *)
let t91 = prob_xw [INS('e','d'); TRANS('h','a')] = 0.001443001443001443;;
let t92 = prob_xw [INS('a','b'); INS('b','a')] = 0.00015532774153463808;;
let t_prob_xw = t91 && t92;;

(* Test de la fonction prob_uwv *)
let t101 = (List.map prob_uwv ["back";"hope";"to"]) =
      [0.044444444444444446; 0.066666666666666666; 0.1111111111111111];;
let t102 = (List.map (fun (x,y) -> prob_uwv ~u:x y) 
           ["hope","to";"realize","to";"i","hope";
            "<s>","hope";"<s>","i"]) =
      [0.15789473684210525; 0.052631578947368418; 0.15; 
       0.045454545454545456; 0.18181818181818182];;
let t103 = (List.map (fun (x,y) -> prob_uwv x ~v:y) 
           ["future","</s>";"dream","</s>";"hope","to";"to","be"]) =
      [0.010526315789473684; 0.010526315789473684; 0.010526315789473684;
       0.010582010582010581];;
let t104 = (List.map (fun (x,y,z) -> prob_uwv ~u:x y ~v:z) 
           ["i","hope","to";"the","future","</s>";
            "to","realize","my";"to","realize","that";
            "the","future","</s>";"<s>","i","have";
            "<s>","i","hope"]) =
      [0.023684210526315787; 0.023684210526315787; 0.015037593984962405;
       0.015037593984962405; 0.023684210526315787; 0.018181818181818184;
       0.027272727272727271];;
let t_prob_uwv = t101 && t102 && t103 && t104;;

(* Test de la fonction find_candidates *)
let t_find_candidates = List.for_all2 (fun l l' -> l <=> l')
        (List.map find_candidates ["hve";"dram";"futuer";"te";"bck";"ope"])
        [[("be", [SUB ('b', 'h'); INS ('v', 'b')]);
          ("hope", [SUB ('o', 'v'); DEL ('p', 'o')]);
          ("the", [DEL ('t', '#'); INS ('v', 'h')]); 
          ("have", [DEL ('a', 'h')])];
         [("dream", [DEL ('e', 'r')]); 
          ("dreams", [DEL ('e', 'r'); DEL ('s', 'm')])];
         [("future", [TRANS ('r', 'e')])];
         [("be", [SUB ('b', 't')]); ("to", [SUB ('o', 'e')]);
          ("in", [SUB ('i', 't'); SUB ('n', 'e')]);
          ("i", [SUB ('i', 't'); INS ('e', 'i')]); 
          ("the", [DEL ('h', 't')]);
        ("is", [SUB ('i', 't'); SUB ('s', 'e')]);
          ("my", [SUB ('m', 't'); SUB ('y', 'e')]);
          ("a", [SUB ('a', 't'); INS ('e', 'a')])];
         [("be", [SUB ('e', 'c'); INS ('k', 'e')]); 
          ("back", [DEL ('a', 'b')])];
         [("be", [SUB ('b', 'o'); INS ('p', 'b')]); 
          ("hope", [DEL ('h', '#')]);
          ("the", [SUB ('t', 'o'); SUB ('h', 'p')])]]

(* Test de la fonction best_candidate *)
let t121 = (List.map (fun c -> best_candidate c (find_candidates c)) 
           ["hve";"dram";"futuer";"te";"bck";"ope"]) =
       ["have"; "dream"; "future"; "to"; "back"; "hope"];;
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
let t_revise_lwords = List.map (revise_lwords % split_line) 
                 ["i hve a dream";"i hpe to be";
                "i hpe to understand the meaning";
                "to reaize my fuure"] =
       [["i"; "have"; "a"; "dream"]; ["i"; "hope"; "to"; "be"];
      ["i"; "hope"; "to"; "understand"; "the"; "meaning"];
      ["to"; "realize"; "my"; "future"]];;

(* Test de la fonction find_cand_aux *)
let t151 = List.map find_cand_aux ["";"i";"bck";"hop";"t";"dram";"drem"] =
      [[("i", 0.18181818181818182); ("the", 0.090909090909090912);
          ("back", 0.090909090909090912)];
       [("i", 0.18181818181818182); ("is", 0.045454545454545456);
          ("in", 0.045454545454545456)];
       [("back", 0.0012285012285012287); 
        ("be", 6.9279904670851165e-006)];
       [("hope", 0.045454545454545456)];
       [("the", 0.090909090909090912); ("to", 0.045454545454545456);
          ("that", 0.045454545454545456)];
       [("dream", 0.00063131313131313126); 
        ("dreams", 9.1494656712048015e-006)];
         [("dream", 0.00186799501867995); 
          ("dreams", 2.7072391575071739e-005)]];;
let t152 = List.map (fun (u,c) -> find_cand_aux ~u:u c) 
          ["is","t";"in","t";"my","dram";"a","dram"] =
       [[("to", 0.1111111111111111); ("the", 0.055555555555555552);
           ("that", 0.055555555555555552)];
       [("the", 0.1111111111111111); ("to", 0.055555555555555552);
          ("that", 0.055555555555555552)];
       [("dream", 0.0007716049382716049); 
        ("dreams", 2.2365360529611736e-005)];
       [("dream", 0.0015432098765432098); 
        ("dreams", 1.1182680264805868e-005)]];;
let t_find_cand_aux = t151 && t152;;

(* RÉSULTATS                                                                  *) 
(******************************************************************************) 
let test_all = 
  t_liste_words && t_wwpf_frequence && t_liste_words' && t_word_pair_list && 
  t_gen_matrix && t_c_cc_gen && t_gen_table && t_p_fault && t_prob_xw && 
  t_prob_uwv && t_find_candidates && t_best_candidate && t_tri_gramme && 
  t_revise_lwords && t_find_cand_aux;;

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
Printf.printf "\trevise_lwords : %b\n" t_revise_lwords;
Printf.printf "\tfind_cand_aux : %b\n\n" t_find_cand_aux;
Printf.printf "\tTOUT EST OK : %b\n\n" test_all;;

(* FIN TESTS                                                                  *) 
(******************************************************************************) 
