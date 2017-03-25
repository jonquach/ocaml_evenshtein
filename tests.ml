(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2017 ---------------------- *)
(* Tests des fonctions à implanter; tests présents dans l'énoncé du Tp.       *)
(* -------------------------------------------------------------------------- *)
#use "spellc.ml";;
open Utiles;;
open Spellc;;

liste_words "vide.txt";;
liste_words "corpus.txt";;

let h1,h2,n1,n2 = wwpf_frequence "vide.txt";;
H.length h1;;
H.length h2;;

let h1,h2,n1,n2 = wwpf_frequence "corpus.txt";;
H.length h1;;
H.length h2;;
print_h id string_of_int h1;;
print_h (fun (w1,w2) -> w1 ^ "," ^ w2) string_of_int h2;;

liste_words' "vide.txt";;
liste_words' "misspell.txt";;

word_pair_list "vide.txt";;
word_pair_list "misspell.txt";;

let subm, delm, insm, tram = gen_matrix "vide.txt";;
pos_diff_zero subm;;
pos_diff_zero delm;;
pos_diff_zero insm;;
pos_diff_zero tram;;

let subm, delm, insm, tram = gen_matrix "misspell.txt";;
pos_diff_zero subm;;
pos_diff_zero delm;;
pos_diff_zero insm;;
pos_diff_zero tram;;

c_cc_gen "";;
c_cc_gen "a";;
c_cc_gen "ab";;
c_cc_gen "abc";;


let th = gen_table "vide.txt";;
H.length th ;;

let th = gen_table "misspell.txt";;
print_h id string_of_int th ;;

_K;;
p_fault (INS('a','b'));;
p_fault (INS('b','a'));;
p_fault (INS('e','d'));;
p_fault (TRANS('h','a'));;

prob_xw [INS('e','d'); TRANS('h','a')];;
prob_xw [INS('a','b'); INS('b','a')];;

_N;;
_V;;
prob_uwv "back";;
prob_uwv "hope";;
prob_uwv "to";;
prob_uwv ~u:"hope" "to";;
prob_uwv ~u:"realize" "to";;
prob_uwv ~u:"i" "hope";;
prob_uwv ~u:"<s>" "hope";;
prob_uwv ~u:"<s>" "i";;
prob_uwv "future" ~v:"</s>";;
prob_uwv "dream" ~v:"</s>";;
prob_uwv "hope" ~v:"to";;
prob_uwv "to" ~v:"be";;
prob_uwv ~u:"i" "hope" ~v:"to";;
prob_uwv ~u:"the" "future" ~v:"</s>";;
prob_uwv ~u:"to" "realize" ~v:"my";;
prob_uwv ~u:"to" "realize" ~v:"that";;
prob_uwv ~u:"the" "future" ~v:"</s>";;
prob_uwv ~u:"<s>" "i" ~v:"have";;
prob_uwv ~u:"<s>" "i" ~v:"hope";;

find_candidates "hve";;
find_candidates "dram";;
find_candidates "futuer";;
find_candidates "te";;
find_candidates "bck";;
find_candidates "ope";;

best_candidate "hve" (find_candidates "hve");;
best_candidate "dram" (find_candidates "dram");;
best_candidate "futuer" (find_candidates "futuer");;
best_candidate "te" (find_candidates "te");;
best_candidate "bck" (find_candidates "bck");;
best_candidate "ope" (find_candidates "ope");;
best_candidate ~u:"i" "hve" (find_candidates "hve");;
best_candidate ~u:"i" "hve" ~v:"to" (find_candidates "hve");;
best_candidate ~u:"in" "te" (find_candidates "te");;
best_candidate ~u:"in" "te" ~v:"future" (find_candidates "te");;

tri_gramme [];;
tri_gramme ["m"];;
tri_gramme ["m1"; "m2"];;
tri_gramme ["m1"; "m2"; "m3"];;

(revise_lwords % split_line) "i hve a dream";;
(revise_lwords % split_line) "i hpe to be";;
(revise_lwords % split_line) "i hpe to understand the meaning";;
(revise_lwords % split_line) "to reaize my fuure";;

find_cand_aux "";;
find_cand_aux "i";;
find_cand_aux "bck";;
find_cand_aux "hop";;
find_cand_aux "t";;
find_cand_aux ~u:"is" "t";;
find_cand_aux ~u:"in" "t";;

find_cand_aux "dram";;
find_cand_aux "drem";;
find_cand_aux  ~u:"my" "dram";;
find_cand_aux  ~u:"a" "dram";;

gen_txt_auto 10;;
