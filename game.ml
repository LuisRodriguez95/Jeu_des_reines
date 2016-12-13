open Gamebase

let size = 5

type piece = Queen | Pawn of int | Empty

type dir = N | NE | E | SE | S | SO | O | NO 

(* These types are abstract in game.mli *)

type state = piece matrix * player 

type move =  piece * dir * int (* piece, direction, distance a deplacer *) 

type result = Win of player


(*Printers *)

   let piece2s p =
     match p with
     | Queen -> "Q"
     | Pawn i ->  string_of_int i 
     | Empty -> " "


   let state2s (m,p) = Printf.sprintf "Current = \n%s\n  // %s to play" (matrix2s m piece2s) (player2s p) 

   let dir2s d = 
     match d with
      | N -> "N"
      | NE -> "NE"
      | NO -> "NO"
      | O -> "O"
      | E -> "E"
      | SE -> "SE"
      | SO -> "SO"
      | S -> "S"



   let move2s (p,d,i) = Printf.sprintf "(%s,%s,%d)" (piece2s p) (dir2s d) i


   let result2s (Win p) = (player2s p) ^ " wins"


(* Reader *)
let readmove s =
  let aux a b c = match b with
   | "N" -> Some (Pawn a, N, c)
   | "NE" -> Some (Pawn a, NE, c)
   | "NO" -> Some (Pawn a, NO, c)
   | "O" -> Some (Pawn a, O, c)
   | "E" -> Some (Pawn a, E, c)
   | "SE" -> Some (Pawn a, SE, c)
   | "SO" -> Some (Pawn a, SO, c)
   | "S" -> Some (Pawn a, S, c)
   | _ -> None
   in
   
   try (Scanf.sscanf s "%d %s %d" aux)
   with _ -> None

(*Fonctions auxiliaires pour le jeu*)

let movement (x,y) dir dist = 
    match dir with
      | S -> (x+dist,y)
      | N -> (x-dist,y)
      | O -> (x,y-dist)
      | E -> (x,y+dist)
      | SE -> (x+dist,y+dist)
      | NE -> (x-dist,y+dist)
      | SO -> (x+dist,y-dist)
      | NO -> (x-dist,y-dist)



let match_pawn i =
	match i with
		| 1 ->  (fun x -> match x with | (Pawn 1) -> true | _ -> false) 
		| 2 ->  (fun x -> match x with | (Pawn 2) -> true | _ -> false) 
		| 3 ->  (fun x -> match x with | (Pawn 3) -> true | _ -> false) 
		| 4 ->  (fun x -> match x with | (Pawn 4) -> true | _ -> false) 
		| 5 ->  (fun x -> match x with | (Pawn 5) -> true | _ -> false) 
		(*| 6 ->  (fun x -> match x with | (Pawn 6) -> true | _ -> false) 
		| 7 ->  (fun x -> match x with | (Pawn 7) -> true | _ -> false) 
		| 8 ->  (fun x -> match x with | (Pawn 8) -> true | _ -> false) *)
		| _ -> assert false


let rec pawn_win m i= 
  if  i=0 then false 
  else 
    match(find_cell m (match_pawn i)) with
      | None -> pawn_win m (i-1) 
      | Some (x,y) -> if x=0 then true else pawn_win m (i-1)



(* You have to provide these. *)
let initial = 
  let init = Array.make_matrix size size Empty in 
	let aux m = 
      m.(0).(2) <- Queen ;
  		m.(4).(2) <- Pawn 1 ;
  		m.((size-1)).(1) <- Pawn 2 ;
  		m.((size-1)).(3) <- Pawn 3 ;
  		(*m.((size-1)).(3) <- Pawn 4 ;
  		m.((size-1)).(4) <- Pawn 5 ; *)
  		(*m.(7).(5) <- Pawn 6 ;
  		m.(7).(6) <- Pawn 7 ;
  		m.(7).(7) <- Pawn 8 ; *)
      m
  	in			
    		(aux init, Human)

let turn (_,p) = p

let is_valid (l,pl) (piece,dir,dist) = 
  	match piece with
    	| Queen -> let coord = find_cell l (fun x -> match x with | Queen -> true | _ -> false) in (* a modidifer car ça renvoie Some coord*)
          		(match coord with

              | None -> false
              | Some coord -> if (inside_matrix (movement coord dir dist)) then true  else false) 
    	| Empty -> false
    	| Pawn i->       
        let coordP = find_cell l (match_pawn i) in (* a modidifer car ça renvoie Some coord*)
        let coordQ = find_cell l (fun x -> match x with | Queen -> true | _ -> false) in
        (match (coordP,coordQ) with
          | (Some (xP,yP),Some (xQ,yQ)) -> if ((inside_matrix (movement (xP,yP) dir dist)) && dist=1 && (dir=N || (dir=NE && (xQ=xP-1 && yQ= yP+1)) || (dir = NO && (xQ = xP-1 && yQ = yP - 1)))) then true  else false 
          | _ -> false)
            	
let play (m,pl) (piece,dir,dist) =  (*state et move en argument et renvoie state*) 
  let newM = clone_matrix m in
  match piece with
    | Queen -> 
    	(let coord = find_cell m (fun x -> match x with | Queen -> true | _ -> false) in (* a modidifer car ça renvoie Some coord, tester le cas None*)
      match coord with
      | None -> raise Not_found
      | Some (x,y) ->
        let (x2,y2) = movement (x,y) dir dist in
        newM.(x2).(y2) <- piece;
        newM.(x).(y) <- Empty;
        (*Printf.printf "passage par ici pour la reine se trouvant a %d %d \n%!" x y ;
        Printf.printf " %s \n%!" (matrix2s newM piece2s) ;*)
        (newM, next pl))
    | Empty -> raise Not_found
    | Pawn i ->
      (let coord = find_cell m (match_pawn i) in (* a modidifer car ça renvoie Some coord, tester le cas None*)
      (match coord with
      | None -> raise Not_found
      | Some (x,y) ->
        let (x2,y2) = movement (x,y) dir dist in
        newM.(x2).(y2) <- piece;
        newM.(x).(y) <- Empty;
        (*Printf.printf "passage par ici pour le pawn %d se trouvant a %d %d \n%!" i x y ;*)
        (newM, next pl)))





let create_moves_comput =
  let l_dist = [1;2;3;4] in
  let l_dir = [N; NE; NO; S; SE; SO; O; E] in
  let rec aux l1 l2 sauv =
    match (l1,l2) with
      | ([],_) -> []
      | (h1::t1,h2::t2) -> (Queen,h1,h2)::(aux (h1::t1) t2 sauv)
      | (h1::t1,[]) -> aux t1 sauv sauv
  in 
    aux l_dir l_dist l_dist


let all_moves (matrix,player)= (* je suppose pour le moment que c'est le human qui bouge les pions.*)
  (* Je pense qu'il faudra rajouter une variable globale ou qqchose comme ca qui nous dise qui joue quoi*)
  	match player with
    		| Human -> [(Pawn 1, N,1); (Pawn 1, NE,1);(Pawn 1, NO,1);
                			(Pawn 2, N,1); (Pawn 2, NE,1);(Pawn 2, NO,1);
                			(Pawn 3, N,1); (Pawn 3, NE,1);(Pawn 3, NO,1);
                			(Pawn 4, N,1); (Pawn 4, NE,1);(Pawn 4, NO,1);
                			(Pawn 5, N,1); (Pawn 5, NE,1);(Pawn 5, NO,1);
                			]	
    		| _ -> create_moves_comput

				  
				  
	
let result (m,player) = 
	if (pawn_win m size) then Some (Win Human) 
  else 
    if ((find_cell m (fun x -> match x with | Queen -> true | _ -> false))=None) then Some (Win Human) 
    else (
    	let rec aux i = 
        match i with
        	| 0 -> Some (Win Comput)
        	| _ -> if ( find_cell m (match_pawn i) = None ) then aux (i-1) else None
      in 
        aux size)


(* This type was given in game.mli.
* We have to repeat it here. *)
type comparison = Equal | Greater | Smaller

let compare p r1 r2 = 
match (r1,r2) with
| (a,b) -> if a=b then Equal else match (r1,r2) with
| (Win g, Win t) -> if (g=p) then Smaller else Greater

let worst_for p = Win (next p)

let best_for p = Win p
;;
