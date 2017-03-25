#use "spellc.ml";;
#load "graphics.cma";;

module Gui2 : 
sig 
  	val go : unit -> string
end = 
struct

	open Spellc

	module G = Graphics
  	module L = List
	module S = Stack
	module Str = String

	let _ = G.open_graph " 550x600"
	let _ = G.set_window_title "Spell-checker"

  	(* Initialisation de quelques variables *)
	let saut_ligne = 20
	let limit_ligne = 50
	let x_txt_size, y_txt_size = G.text_size "A"
	let saut = ref 1
	let sauts = S.create()

  	(* Séparateurs de mots *)
	let sep_eol = [';'; '.'; '!'; '?']
	let sep_inl = [' '; ','; '\r']
	let sep = sep_eol @ sep_inl

  	(* Couleurs de fond barre de status et barre de suggestions *)
	let status_back_color = G.rgb 105 105 105
	let status_fore_color = G.white
	let suggestions_back_color = G.rgb 192 192 192
	let suggestions_fore_color = G.foreground

	let beep () =
	  G.sound 700 5

	let new_line () = 
	  incr saut;
	  G.moveto x_txt_size (G.size_y() - (!saut * saut_ligne))

	let show_cursor () =
	  let x',y' = G.current_x(), G.current_y() in
	  G.fill_rect x' (y'-3) x_txt_size 1

	let clear_cursor () =
	  G.set_color G.background; 
	  show_cursor();
	  G.set_color G.foreground

	let show_stat n cur last =
	  let x',y' = G.current_x(), G.current_y() in
	  G.set_color status_back_color; 
	  G.fill_rect 0 0 (G.size_x()) 23; 
	  G.set_color status_fore_color;
	  G.moveto x_txt_size 5;
	  G.draw_string ((string_of_int n) ^ " suggestion(s) for \"" ^ 
	                                     cur ^ "\" considering \"" ^ last ^ "\"");
	  G.set_color G.foreground;
	  G.moveto x' y'
	 

	let init () =
	  G.clear_graph();
	  (* Suggestions bar *)
	    G.set_color suggestions_back_color; 
	    G.fill_rect 0 (G.size_y() - 40) (G.size_x()) 40;  
	    G.set_color G.foreground; 
	    G.fill_rect 0 (G.size_y() - 40) (G.size_x()) 1;
	  (* Status bar *)
	    G.set_color status_back_color; 
	    G.fill_rect 0 0 (G.size_x()) 24;  
	    G.set_color (G.rgb 192 192 192); 
	    G.fill_rect 0 23 (G.size_x()) 1;
	    G.set_color G.foreground;
	  saut := 3;
	  new_line();
	  show_cursor()

	let rec backspace n =
	  let x' = G.current_x() - (n * x_txt_size) in
	  if x' >= x_txt_size then
	    (* Effacement sur une ligne *)
	    begin
	      let y' = G.size_y() - (!saut * saut_ligne) in
	      G.moveto x' y';
	      G.set_color G.background; 
	      G.fill_rect x' y' (n * x_txt_size) y_txt_size; 
	      G.set_color G.foreground
	    end
	  else
	  (* Effacement sur plusieurs lignes *)
	    let n1 = (G.current_x() - x_txt_size) / x_txt_size in
	    let n2 = n - n1 - 1 in
	    let _ = backspace n1 in
	    if (!saut > 4) then
	      begin
	        decr saut; 
	        G.moveto (S.pop sauts) (G.size_y() - (!saut * saut_ligne));
	        backspace n2  
	      end

	let clear_candidates () =
	  G.set_color suggestions_back_color; 
	  G.fill_rect 0 (G.size_y() - 30) (G.size_x()) y_txt_size; 
	  G.set_color G.foreground

	let show_candidates l pos =
	  let _ = clear_candidates() in
	  if l <> [] && pos >= 0 && pos < L.length l then
	    begin
	      let x',y' = G.current_x(), G.current_y() in
	      let _ = G.moveto (x_txt_size * 3) (G.size_y() - 30) in
	      let _ = G.set_color G.blue in
	      let _ = G.draw_string (L.nth l pos) in
	      let _ = G.set_color G.foreground in
	      let n = L.length l in
	      let rec aux pos' n' =
	        let w = L.nth l pos' in
	        let str_size = fst (G.text_size w) + 3 in
	        if n' >= 1 &&
	           (G.current_x()) + str_size + (3 * x_txt_size) <= G.size_x() 
	        then
	          begin
	            G.set_color suggestions_fore_color;
	            G.draw_string ("   " ^ w);
	            G.set_color G.foreground;
	            aux ((pos'+1) mod n) (n'-1)
	          end
	      in
	      aux ((pos+1) mod n) (n-1);
	      G.moveto x' y'
	    end
	

  	(* Affiche correctement un string, et fait saut de ligne si nécessaire *)
	let print_str str =
	  let _ = clear_cursor() in
	  let x_str_size, _ = G.text_size str in
	  if G.current_x() > (G.size_x ()) - (x_str_size + x_txt_size) then
	    begin
	      S.push (G.current_x()) sauts;
	      new_line()
	    end;
	  G.moveto (G.current_x()) (G.size_y() - (!saut * saut_ligne));
	  G.draw_string str;
	  show_cursor ()

  	(* n pop d'une stack *)
  	let npop st n =
  	  let rec aux n = 
      	if n >= 1 then
      		(ignore(S.pop st); aux (n-1))
  	  in
      aux n

 	(* Retourne le string en tête de la pile; si le sommet est un séparateur
       retourne chaine vide *)
	let last_w (skip_sep,st) =
	  let rec aux st w = 
	    if (not (S.is_empty st)) && (not (L.mem (S.top st) sep)) then
	      let c = S.pop st in aux st ((Str.make 1 c) ^ w)
	    else
	      st, w
	  in
	  if skip_sep then
	    begin
	      while (not ((S.is_empty st) || (L.mem (S.top st) sep_eol))) 
	              && (L.mem (S.top st) sep_inl) do 
	        ignore (S.pop st) 
	      done   
	    end;
	  aux st ""

  	(* Affiche le contenu de la pile *)
	let txt_in st =
	  let n = S.length st in 
	  let txt = Str.make (n) ' ' in
	  let i = ref (n-1) in
	  S.iter (fun c -> Bytes.set txt (!i) (if c = '\r' then '\n' else c); decr i) st;
	  txt

  	(* Foncton principale. 
       Rq: si un mot saisi ou auto-complété dépasse la taille de la ligne,
       il passe auto. à la ligne suivante et affiche toute la string à 
       nouveau pour continuer traitement.
  	*)
	let input f =
	  let rec aux st l_cand pos =
      	(* Fonction permettant de boucler tant que touche Esc non saisie *)
	    let key = G.read_key() in
	    match key with
	    | '\027' (* Esc *) ->
	        show_candidates ["FINISH!"] 0;
	        show_stat 0 "" "End of job!";
	        txt_in st
	    | '\r' ->
	        if G.size_y() - ((!saut + 1) * saut_ligne) > limit_ligne then       
	          let _, last_str = last_w (true,S.copy st) in 
	          let l_cand = f (last_str, "") in
	            show_candidates l_cand 0;
	            show_stat (L.length l_cand) "" last_str;
	            S.push (G.current_x()) sauts;
	            clear_cursor();
	            new_line();
	            show_cursor ();
	            S.push key st;
	            aux st l_cand 0 
	        else
	          (beep(); aux st l_cand pos)
	    | '\b' ->
	        if not (S.is_empty st) then
	          let _ = S.pop st in
	          let _ = clear_cursor() in
	          let _ = backspace 1 in
	          let _ = show_cursor () in
	          let st',current_str = last_w (false,S.copy st) in 
	          let _, last_str = last_w (true,st') in
	          let l_cand = f (last_str, current_str) in
	          let _ = show_candidates l_cand 0 in
	          let _ = show_stat (L.length l_cand) current_str last_str in
	          aux st l_cand 0
	        else
	          begin
	            beep();
	            aux st l_cand pos
	          end
	    | '\t' ->
	        begin
	          match l_cand with
	          | [] -> 
	            beep(); 
	            aux st l_cand pos 
	          | _-> 
	            let cand = L.nth l_cand pos in
	            let n = if (S.is_empty st) || (L.mem (S.top st)) sep 
	                    then 0 
	                    else Str.length (snd (last_w (false,S.copy st))) in
	            let cand_size = fst(G.text_size cand) in
	            if (G.current_x() + cand_size + ((1-n) * x_txt_size) <= G.size_x())
	               || 
	               (G.size_y() - ((!saut + 1) * saut_ligne) > limit_ligne)
	            then 
	              begin
	                clear_cursor();
	                backspace n;
	                npop st n;
	                print_str cand;
	                Str.iter (fun c -> S.push c st) cand;
	                let pos = (pos + 1) mod (L.length l_cand) in
	                begin
	                  show_candidates l_cand pos;                
	                  aux st l_cand pos 
	                end  
	              end
	            else
	              begin
	                beep();
	                let pos = (pos + 1) mod (L.length l_cand) in
	                  show_candidates l_cand pos;                
	                  aux st l_cand pos 
	              end
	       end    
	    | _ ->
	        if (G.current_x() + x_txt_size + x_txt_size <= G.size_x())
	           || 
	           (G.size_y() - ((!saut + 1) * saut_ligne) > limit_ligne)
	        then 
	          let sKey = Str.make 1 key in
	          let _ = clear_cursor() in
	          let _ = S.push key st in
	          let _ = print_str sKey in
	          let st',current_str = last_w (false,S.copy st) in 
	          let _, last_str = last_w (true,st') in
	          let l_cand = f (last_str, current_str) in
	          let _ = show_candidates l_cand 0 in
	          let _ = show_stat (L.length l_cand) current_str last_str in
	          aux st l_cand 0 
	        else
	          (beep(); aux st l_cand pos)
	  in
	  let l_cand = f ("", "") in
	  show_candidates l_cand 0;
	  show_stat (L.length l_cand) "" "";
	  aux (S.create()) l_cand 0
	
  	let go() =
      begin
      	init();
      	input (fun (u,w) -> find_cand ~u:u w)
      end 
  	
end;;

open Gui2;;

go()
 