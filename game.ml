open Gamebase
open Images
open Png
open Graphic_image 

let size = 5

(*Different pieces on board*)
type piece = Queen | Pawn of int | Empty

(*All different directions for a pawn to move*)
type dir = N | NE | E | SE | S | SO | O | NO 

(* These types are abstract in game.mli *)

type state = piece matrix * player 

type move =  piece * dir * int (* piece, direction, distance *) 

type result = Win of player

(* Getters *)
let getMatrix (m,_) = m 

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
(*Transforms a move from string to move*)
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

(*Auxiliary function that virtually computes a movement and gives us the new coordinates *)
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

(*Function that allows to test if a certain pawn is on board*)
let match_pawn i =
	match i with
		| 1 ->  (fun x -> match x with | (Pawn 1) -> true | _ -> false) 
		| 2 ->  (fun x -> match x with | (Pawn 2) -> true | _ -> false) 
		| 3 ->  (fun x -> match x with | (Pawn 3) -> true | _ -> false) 
		| 4 ->  (fun x -> match x with | (Pawn 4) -> true | _ -> false) 
		| 5 ->  (fun x -> match x with | (Pawn 5) -> true | _ -> false) 
		| 6 ->  (fun x -> match x with | (Pawn 6) -> true | _ -> false) 
		| 7 ->  (fun x -> match x with | (Pawn 7) -> true | _ -> false) 
		| 8 ->  (fun x -> match x with | (Pawn 8) -> true | _ -> false) 
		| _ -> assert false

(*If there is still pawns on board then the queen has won*)
let rec pawn_win m i= 
  if  i=0 then false 
  else 
    match(find_cell m (match_pawn i)) with
      | None -> pawn_win m (i-1) 
      | Some (x,y) -> if x=0 then true else pawn_win m (i-1)


let initial = 
  let init = Array.make_matrix size size Empty in 
	let aux m = 
      m.(0).(2) <- Queen ;
  		m.((size-1)).(0) <- Pawn 1 ;
  		m.((size-1)).(1) <- Pawn 2 ;
  		m.((size-1)).(2) <- Pawn 3 ;
  		m.((size-1)).(3) <- Pawn 4 ;
  		(*m.((size-1)).(4) <- Pawn 5 ;*)
  		(*m.(7).(5) <- Pawn 6 ;
  		m.(7).(6) <- Pawn 7 ;
  		m.(7).(7) <- Pawn 8 ; *)
      m
  	in			
    		(aux init, Human)

let turn (_,p) = p

(*Forbids the queen to step over a Pawn*)
let rec not_teleport m (x,y) dir dist =
  if dist = 0 then true
  else
    let piece = m.(x).(y) in
    match piece with
      |Pawn i -> false
      | _ ->  (match dir with
          | S -> not_teleport m (x+1,y) dir (dist-1)
          | N -> not_teleport m (x-1,y) dir (dist-1)
          | O -> not_teleport m (x,y-1) dir (dist-1)
          | E -> not_teleport m (x,y+1) dir (dist-1)
          | SE -> not_teleport m (x+1,y+1) dir (dist-1)
          | NE -> not_teleport m (x-1,y+1) dir (dist-1)
          | SO -> not_teleport m (x+1,y-1) dir (dist-1)
          | NO -> not_teleport m (x-1,y-1) dir (dist-1))



(*Function which defines if a movement is valid *)
let is_valid (m,pl) (piece,dir,dist) = 
  	match piece with
    	| Queen -> let coord = find_cell m (fun x -> match x with | Queen -> true | _ -> false) in 
          		(match coord with
              | None -> false
              | Some coord -> if (inside_matrix (movement coord dir dist)) && (not_teleport m coord dir dist) then true  else false)
    	| Empty -> false
    	| Pawn i->       
        let coordP = find_cell m (match_pawn i) in 
        let coordQ = find_cell m (fun x -> match x with | Queen -> true | _ -> false) in
        (match (coordP,coordQ) with
          | (Some (xP,yP),Some (xQ,yQ)) -> if ((inside_matrix (movement (xP,yP) dir dist)) && dist=1 && (dir=N || (dir=NE && (xQ=xP-1 && yQ= yP+1)) || (dir = NO && (xQ = xP-1 && yQ = yP - 1)))) then 
                                              true  (* Tries the different moves of a pawn: generally N, NO and NE but with some exceptions*)
                                           else false 
          | _ -> false)

(*Function that plays a move *)            	
let play (m,pl) (piece,dir,dist) = 
  let newM = clone_matrix m in
  match piece with
    | Queen -> 
    	(let coord = find_cell m (fun x -> match x with | Queen -> true | _ -> false) in 
      match coord with
      | None -> raise Not_found
      | Some (x,y) ->
        let (x2,y2) = movement (x,y) dir dist in
        newM.(x2).(y2) <- piece;
        newM.(x).(y) <- Empty;
        (newM, next pl))
    | Empty -> raise Not_found
    | Pawn i ->
      (let coord = find_cell m (match_pawn i) in 
      (match coord with
      | None -> raise Not_found
      | Some (x,y) ->
        let (x2,y2) = movement (x,y) dir dist in
        newM.(x2).(y2) <- piece;
        newM.(x).(y) <- Empty;
        (newM, next pl)))

(* Auxiliary function used by drawn to guess if there is just one pawn on board and to have its coordinates *)
let rec pions m acu i coord =
  if  i=0 then (if acu=1 then (true,coord) else (false,coord) ) (*si i=0 alors on cherche tous les pions*)
  else                                                          (*si acu=1 alors on a un seul pion dans le jeu*)
    match(find_cell m (match_pawn i)) with                        
      | None -> pions m acu (i-1) coord
      | Some (x,y) -> pions m (i+1) (i-1) (x,y)

(* To test if the game ends in a drawn. We suppose that if there is a drawn then Human wins. *)
let drawn (m,player) = 
	if player = Comput then false 
	else
		let coord = find_cell m (fun x -> match x with | Queen -> true | _ -> false) in
		match coord with
			| None -> false
			| Some (xQ,yQ) -> 
				let (good,(xP,yP)) = pions m 0 size (0,0) in
					if good && xQ=(xP-1) && yQ=yP then true else false 
		
(* Auxiliary function to create a matrix with all possible moves for the queen *)
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


let all_moves (matrix,player)= 

  	match player with
    		| Human -> [(Pawn 1, N,1); (Pawn 1, NE,1);(Pawn 1, NO,1);
                			(Pawn 2, N,1); (Pawn 2, NE,1);(Pawn 2, NO,1);
                			(Pawn 3, N,1); (Pawn 3, NE,1);(Pawn 3, NO,1);
                			(Pawn 4, N,1); (Pawn 4, NE,1);(Pawn 4, NO,1);
                			(Pawn 5, N,1); (Pawn 5, NE,1);(Pawn 5, NO,1);
                			]	
    		| _ -> create_moves_comput



let result (m,player) = 
	if drawn (m,player) then Some (Win Human)
	else
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

(*let compare p r1 r2 = 
match (r1,r2) with
| (a,b) -> if a=b then Equal else match (r1,r2) with
| (Win g, Win t) -> if (g=p) then Smaller else Greater*)

(* Compare function used by map_fold_ac that compares 2 results *)

let compare_fold (m1,r1) (m2,r2) = 
 if r1 = Win Comput then (m1,Win Comput) else if r2 = Win Comput then (m2,Win Comput) else (m2,Win Human)

let worst_for p = Win (next p)

let best_for p = Win p



(*Graphic functions *)
let dim_fenetre = size* 100


(*Open images*)

let queen = load_as_rgb24 "reine.png" []


(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)


(*Event when the user clicks on the left button of his mouse *)
let event_d ()  =
	let event_down = Graphics.wait_next_event  [Graphics.Button_down] in
	event_down

(*Event when the user releases the left button of his mouse *)
let event_u ()  =
	let event_up = Graphics.wait_next_event  [Graphics.Button_up] in
	event_up

(*Transforms the position x and y in the map, into the relative coordinates in the matrix (axis are not the same)  *)
let transform_position pos_x pos_y = 
	let transform_x x = x/((Graphics.size_x())/size) in
	let transform_y y = y/((Graphics.size_y())/size) in
	((size-1) - transform_y pos_y, (transform_x pos_x)) 


(*Match if a pawn is on a specific coordinate (x,y) of the matrix *)
let check_pawn m (x,y) = 
match m.(x).(y) with 
	| Pawn a ->  true 
	| p -> false 

(*Sub distances *)
let sub_distance c_old c_new = (c_new-c_old)

(*Return the number of a pawn *)
let get_int_of_pawn m x_old y_old=
	match m.(x_old).(y_old) with
	| Pawn i -> i 
	| _ -> -1 

(*Returns a string with the correct direction of the move *)
let orientation m (x_old, y_old) (x_new, y_new) =
	let diff_x = sub_distance x_old x_new in
	let diff_y = sub_distance y_old y_new in
		if diff_x > 0 then 
			if diff_y > 0 then 
				 string_of_int (get_int_of_pawn m x_old y_old) ^ " SE " ^   string_of_int (abs diff_x)
			else
				if diff_y = 0 then
					string_of_int (get_int_of_pawn m x_old y_old) ^ " S " ^  string_of_int (abs diff_x)
				else
					 string_of_int (get_int_of_pawn m x_old y_old) ^ " SO " ^  string_of_int (abs diff_x)
		else
			if diff_x = 0 then
				if diff_y > 0 then
					  string_of_int (get_int_of_pawn m x_old y_old) ^ " E " ^  string_of_int (abs diff_y)
				else
					if diff_y = 0 then 
						 string_of_int (get_int_of_pawn m x_old y_old) ^ " N " ^  string_of_int (abs diff_x) 
					else
						 string_of_int (get_int_of_pawn m x_old y_old) ^ " O " ^  string_of_int (abs diff_y)
			else
				if diff_y > 0 then
						 string_of_int (get_int_of_pawn m x_old y_old) ^ " NE " ^  string_of_int (abs diff_y)
					else
						if diff_y = 0 then 
							 string_of_int (get_int_of_pawn m x_old y_old) ^ " N " ^  string_of_int (abs diff_x) 
						else
							 string_of_int (get_int_of_pawn m x_old y_old) ^ " NO " ^  string_of_int (abs diff_y)

(*Match if a pawn is placed in a specific position of the matrix*)
let which_pawn  m (x,y) = 
match m.(x).(y) with 
	| Pawn a ->  a 
	| p -> assert false 

(*Transforms coordinates of the matrix into coordinates in the map*)
let transform_coordinates (x,y) =
let transform_x x = x*((Graphics.size_x())/size) in
let transform_y y = y*((Graphics.size_y())/size) in
(transform_y y, transform_x ((size-1)- x))

(*Display in green valid movements in the map*)
let rec valid_places m l (x,y) i =
	 match l with
	| [] -> ()
	| p::reste ->  (match p with 
				| (Pawn a, dir , dist) ->  
					if a = i then 
						let (xs,ys) = movement (x,y) dir dist in 
						Graphics.set_color Graphics.green;
						Graphics.fill_circle  (ys*100 +50) ((size-xs-1)* 100 +50) 25 ; valid_places m reste (x,y) i 
					else
						valid_places m reste (x,y) i 
						
				| _ -> valid_places m reste (x,y) i )
					
						
			
		 	
			
(*Drag and drop function *)
let rec drag_n_drop state =
	let event_down= event_d () in (*Wait for the event*)
	let (xd,yd) = transform_position event_down.Graphics.mouse_x event_down.Graphics.mouse_y in  (*Transforms position *)
	let m = getMatrix state in 
	match check_pawn m (xd,yd) with (*Check if it is a pawn *)
	| false -> 	Printf.printf"Sélectionnez un pion, s'il-vous-plait\n%!" ;
			 drag_n_drop state
	| true -> 
			let i = which_pawn m (xd,yd) in (*Check which pawn has been selected *)
			valid_places  m (List.filter (is_valid state) (all_moves state)) (xd,yd) i ; (* Display in green valid places for the selected pawn *)
			let  event_up = event_u () in (*Wait for the event *)
			let (xu,yu) = transform_position event_up.Graphics.mouse_x event_up.Graphics.mouse_y in (*Transforms position *)
			orientation m (xd,yd) (xu,yu) (*Deducts the orientation and returns a string corresponding to the move made with the drag and drop *)


(*Getters*)
let getX () = (Graphics.size_x ())
let getY () = (Graphics.size_y ())

(*Draw all the squares of a line *)
let rec draw_rects_line longueur_ligne y_ligne acu ecart  = 
	match acu with 
	| 0 -> Graphics.draw_rect longueur_ligne y_ligne ecart ecart
	| n  -> 
			Graphics.draw_rect longueur_ligne y_ligne ecart ecart ; 
			draw_rects_line (longueur_ligne-ecart) y_ligne (acu - 1) ecart 
			
(*Draws all the lines and the columns of the grid *)
let rec draw_all y_ligne acu ecart = 
		match acu with
		| 0 ->draw_rects_line (Graphics.size_x ()) 0 size ((Graphics.size_x ())/size) 
		| n -> draw_rects_line (Graphics.size_x ())  y_ligne size ((Graphics.size_x ())/size) ; draw_all (y_ligne-ecart) (acu - 1) ecart

(*Draws the grid *)
let draw () = 
	draw_all (Graphics.size_y ()) size ((Graphics.size_y ())/size)


(*Draws the pawns *)
let rec draw_pawns m i = 
if i = 0 then ()
 else
	match (find_cell m (match_pawn i)) with
	| None -> draw_pawns m (i-1)
	| Some (x,y) ->
				Graphics.set_color Graphics.red;
				Graphics.fill_circle  (y*100 +50) ((size-x-1)* 100 +50) 25 ;
				draw_pawns m (i-1)
				
(*Draws the queen *)				
let draw_queen m = 
	match find_cell m (fun x -> match x with | Queen -> true | _ -> false) with
	| None -> ()
	| Some (x,y) -> draw_image queen (y*100 + 35) ((size-x-1)* 100 +25)
		(*Graphics.set_color Graphics.black;
		Graphics.fill_rect  (y*100 +50) ((size-x-1)* 100 +25) 12 60 ;
		Graphics.fill_rect  (y*100 +25) ((size-x-1)* 100 +55) 60 12 *)

(*Graphic function executed initially*)
let graphic_initial () = 
		Graphics.open_graph "";
		Graphics.set_window_title" Jeu des reines";
		Graphics.resize_window dim_fenetre dim_fenetre

(*Displays the graphic interface for the current state *)
let state_to_graphic state =
	Graphics.clear_graph ();
	Graphics.set_color Graphics.black;
	draw ();
	draw_pawns (getMatrix state) size ; 
	draw_queen (getMatrix state) 
			
