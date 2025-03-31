#load "graphics.cma";;
open Graphics;;

(*Version graphique à faire en dernier !*)

let plateau i j =
   let u = i * 50 and v = j * 50 in
      let s = " " ^ (string_of_int (u + 1)) ^ "x" ^ (string_of_int (v + 1)) ^ "+0+" ^ (string_of_int v) in
         open_graph s;
         clear_graph ();
         set_window_title "MORPION";
         for p = 0 to j do
            (moveto 0 (p * 50); lineto (i * 50) (p * 50));
         done;
         for l = 0 to i do
            (moveto (l * 50) 0; lineto (l * 50) (j * 50))
         done;;

let mat_croix = Array.make_matrix 49 49 red;;

let mat_cercle = Array.make_matrix 49 49 blue;;

let jeu i j =
   plateau i j;
   let plat = plat i j in
      while true do
         if button_down () then
            match mouse_pos () with | (x, y) -> draw_image (make_image mat_cercle) (x - (x mod 50)) (y - (y mod 50));
               play plat (1 + ((x - (x mod 50)) mod 50)) (1 + ((y - (y mod 50)) mod 50))
      done;;

jeu 3 3;;

(*Version technique du jeu*)

let plat n p = let matrice = Array.make_matrix (n + 2) (p + 2) [||] in
      for i = 0 to n + 1 do
         for j = 0 to p + 1 do
            let t = Array.make 9 0 in
               matrice.(i).(j) <- t
         done
      done;
      for i = 0 to n + 1 do
         matrice.(i).(0).(0) <- - 1;
         matrice.(i).(p + 1).(0) <- - 1
      done;
      for j = 1 to p do
         matrice.(0).(j).(0) <- - 1;
         matrice.(n + 1).(j).(0) <- - 1
      done;
      matrice.(0).(0).(1) <- - 1;
      matrice.(0).(0).(7)<- - n;
      matrice.(0).(0).(8)<- - p;
      matrice.(0).(0).(6)<- -n*p;
      matrice;;

(*le joueur 1 commence, cela est indiqué par -1 dans (0).(0).(1)
si c'est au tour du jouer 2 c'est indiqué par un -2 au même emplacement*)

type statut =
	|Nulle
	|Gagnant
	|En_cours;;

let statut_pos pos =
	if pos.(0).(0).(6)=0 && pos.(0).(0).(0)= -1  then Nulle else
	if pos.(0).(0).(0) <> -1 then Gagnant
	else En_cours;;

let deplacement = [|[|0; 0|]; [|-1; 1|]; [|0; 1|]; [|1;1|]; [|1;0|]; [|1; -1|]; [|0; -1|]; [|-1; -1|]; [|-1;0|]|];;

let oppose a =
   if a > 0 && a < 5 then a + 4 else a - 4;;

let autrejoueur plateau =
   let valeur = plateau.(0).(0).(1) in
      if valeur = - 1 then plateau.(0).(0).(1) <- - 2
      else plateau.(0).(0).(1) <- - 1;;

let majligne posi i j valeur direction b =
   let rec aux posi i j acc =
      if posi.(i).(j).(0) = valeur
      then (posi.(i).(j).(oppose direction) <- acc;
            aux posi (i + deplacement.(direction).(0)) (j + deplacement.(direction).(1)) (b + 1))
   in aux posi i j b;;

let majplat posi i j valeur =
   for a = 1 to 8 do
      if posi.(i + deplacement.(a).(0)).(j + deplacement.(a).(1)).(0) = valeur
      then posi.(i).(j).(a) <- 1 + posi.(i + deplacement.(a).(0)).(j + deplacement.(a).(1)).(a)
   done;
   for a = 1 to 8 do
      let b = 1 + posi.(i).(j).(oppose a) in
         majligne posi (i + deplacement.(a).(0)) (j + deplacement.(a).(1)) valeur a b
   done;;

let alignement posi i j align =
   for a = 1 to 8 do
      if 1 + posi.(i).(j).(a) + posi.(i).(j).(oppose a) = align
      then posi.(0).(0).(0) <- (- posi.(0).(0).(1))
   done;;

let play posi align i j =
	if posi.(i).(j).(0) = 0 then
	let valeur=(-posi.(0).(0).(1))
	and vide = posi.(0).(0).(6) in
   posi.(i).(j).(0) <- valeur;
   posi.(0).(0).(6) <- vide+1;
   majplat posi i j valeur;
   alignement posi i j align;
   autrejoueur posi;;

(*posi.(0).(0).(0) = (- 2) then (print_string ("Partie gagnée par joueur " ^ (string_of_int valeur)); print_char (char_of_int 10))
   else (print_string ("Tour du joueur " ^ (string_of_int valeur)); print_char (char_of_int 10));;*)



let unmajligne posi i j direction =
   let a = posi.(i).(j).(0) in
      let rec aux posi i j acc =
         if a = posi.(i + deplacement.(direction).(0)).(j + deplacement.(direction).(1)).(0) then
         (posi.(i + deplacement.(direction).(0)).(j + deplacement.(direction).(1)).(oppose direction) <- acc;
         aux posi (i + deplacement.(direction).(0)) (j + deplacement.(direction).(1)) (acc + 1))
      in aux posi i j 0;;


let unplay posi i j =
   if posi.(i).(j).(0) <> 0 then
      posi.(0).(0).(6) <- posi.(0).(0).(6) - 1;
      if (statut_pos posi = Gagnant) then (posi.(0).(0).(0) <- - 1) else ();
   	for a = 1 to 8 do
      	posi.(i).(j).(a) <- 0;
      	unmajligne posi i j a;
  		done;
   	posi.(i).(j).(0) <- 0;
   	autrejoueur posi;;

	
(*mémoïsation, profondeur, heuristique, élagage alpha-bêta*)


(*Fonction d'évalution*)

let rec puissance x n =
	if n=0 then 1
	else x*(puissance x (n-1));;

let evaluate2 pos =
   let score = ref 0  in
      if statut_pos pos = En_cours then
         if (- pos.(0).(0).(6)) = 0 then score := 0
         else
         	begin
            for i = 1 to (- pos.(0).(0).(7)) do
               for j = 1 to (- pos.(0).(0).(8)) do
                  for n = 1 to 8 do
                  let mouv = pos.(i + deplacement.(n).(0)).(j + deplacement.(n).(1)).(n) in
                        if pos.(i + deplacement.(n).(0)).(j + deplacement.(n).(1)).(0) = 1 then
                           if pos.(i + deplacement.(oppose n).(0)).(j + deplacement.(oppose n).(1)).(0) = 1 then ()
                           else if pos.(i + deplacement.(oppose n).(0)).(j + deplacement.(oppose n).(1)).(0) = 0 then
                              if pos.(i + mouv * deplacement.(n).(0)).(j + mouv * deplacement.(n).(1)).(0) = 0 then
                                 score := !score + puissance 10 (pos.(i).(j).(n) + 2)
                              else if pos.(i + mouv * deplacement.(n).(0)).(j + mouv * deplacement.(n).(1)).(0) = 0 then
                              score := !score + puissance 10 (pos.(i).(j).(n) + 1)
                           else ()
                           else score := !score + puissance 10 (pos.(i).(j).(n))
                        else if pos.(i + deplacement.(oppose n).(0)).(j + deplacement.(oppose n).(1)).(0) = 2 then ()
                           else if pos.(i + deplacement.(oppose n).(0)).(j + deplacement.(oppose n).(1)).(0) = 0 then
                              if pos.(i + mouv * deplacement.(n).(0)).(j + mouv * deplacement.(n).(1)).(0) = 0 then
                                 score := !score - puissance 10 (pos.(i).(j).(n) + 2)
                              else if pos.(i + mouv * deplacement.(n).(0)).(j + mouv * deplacement.(n).(1)).(0) = 0 then
                              score := !score - puissance 10 (pos.(i).(j).(n) + 1)
                           else ()
                           else score := !score - puissance 10 (pos.(i).(j).(n))
                  done; done; done;
             end
else if (statut_pos pos = Gagnant && -pos.(0).(0).(1)=1) then score := - max_int else score := max_int;
!score;;

let pos = plat 3 3 in (play pos 3 1 1;play pos 3 3 1;play pos 3 1 2;play pos 3 3 2);evaluate2 pos ;;

let evaluate pos =
	let score = ref 0 in
      if statut_pos pos = En_cours then score:=0
      else if statut_pos pos = Gagnant then 
      if -pos.(0).(0).(1)=1 then score:= -max_int
      else score:= max_int
      else score:=0;
      !score;;

let pos = plat 3 3 in (play pos 1 1;play pos 1 2;play pos 1 3;play pos 2 2;play pos 2 1;play pos 3 1);evaluate pos;;

let other player =
	if player=1 then 2 else 1;;


(* Minimax avec évaluation et profondeur *)

let rec minimax pos depth align is_maximizing_player =
   if depth <= 0 || statut_pos pos = Nulle || statut_pos pos = Gagnant then
      evaluate pos
   else
      let best_score = ref (if is_maximizing_player then - max_int else max_int) in
         for i = 1 to (- pos.(0).(0).(7)) do
            for j = 1 to (- pos.(0).(0).(8)) do
               if pos.(i).(j).(0) = 0 then
                  (
                  play pos align i j;
                     if is_maximizing_player then
                  		(
                        best_score := max (!best_score) (minimax pos (depth - 1) align false)
                        )
                     else 
                     (
                     best_score := min (!best_score) (minimax pos (depth - 1) align true)
                     );
                     unplay pos i j
                     );
            done; done;
         !best_score;;
   
let find_best_move pos depth align =
   let player = - pos.(0).(0).(1) and
   best_move = ref (- 1, - 1) in
  	let best_score = ref (if player=1 then -max_int else max_int) in
      for i = 1 to (-pos.(0).(0).(7)) do
         for j = 1 to (-pos.(0).(0).(8)) do
            if pos.(i).(j).(0) = 0 then
               (play pos align i j;
                  let move_score = minimax pos (depth - 1) align (player=2) in
                     (unplay pos i j;
                        if (player = 1 && (move_score > !best_score)) || (player = 2 && (move_score < !best_score)) then
                           (best_score := move_score; best_move := (i, j))))
         done;
      done;
    !best_move;;

   
 (*minimax avec élagage alpha-beta*)


exception Break;;

let rec elagage pos depth alpha beta align is_maximizing_player =
	let player = - pos.(0).(0).(1) in
   if depth <= 0 || statut_pos pos = Nulle || statut_pos pos = Gagnant then
      evaluate pos
   else
   let best_score = ref (if is_maximizing_player then -max_int else max_int) and
 	alpha_ref = ref alpha and
 	beta_ref = ref beta in
 	try
   for i=1 to (-pos.(0).(0).(7)) do
   	for j=1 to (-pos.(0).(0).(8)) do
   		if pos.(i).(j).(0)=0 then 
   		let move_score = elagage pos (depth - 1) !alpha_ref !beta_ref align (player=2) in
   		(
   		play pos align i j;
   		if is_maximizing_player then 
   		(best_score:= max !best_score (elagage pos (depth-1) !alpha_ref !beta_ref align false);
   		alpha_ref:= max !best_score !alpha_ref)
   		else (best_score:= min !best_score (elagage pos (depth-1) !alpha_ref !beta_ref align true);
   		beta_ref:= max !best_score !beta_ref);
   		unplay pos i j;
   		if !beta_ref <= !alpha_ref then raise Break;
   		if (player = 1 && (move_score >= !best_score)) || (player = 2 && (move_score <= !best_score)) then
                           (best_score := move_score)
   		);
   	done;done;
   !best_score
   with Break -> !best_score;;

let find_best_move2 pos depth align =
   let player = - pos.(0).(0).(1) and
   best_move = ref (- 1, - 1) in
  	let best_score = ref (if player=1 then -max_int else max_int) in
      for i = 1 to (-pos.(0).(0).(7)) do
         for j = 1 to (-pos.(0).(0).(8)) do
            if pos.(i).(j).(0) = 0 then
               (play pos align i j;
                  let move_score = elagage pos (depth - 1) (-max_int) max_int align (player=2) in
                     (unplay pos i j;
                        if (player = 1 && (move_score > !best_score)) || (player = 2 && (move_score < !best_score)) then
                           (best_score := move_score; best_move := (i, j))))
         done;
      done;
    !best_move;;

let pos = plat 3 3 in play pos 3 1 1;play pos 3 3 1; play pos 3 1 2;play pos 3 3 2; find_best_move2 pos 9 3;;

let pos = plat 3 3 in elagage pos 9 (-max_int) max_int 3 true;;


























