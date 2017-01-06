open Game
open Functory.Network
open Functory.Network.Same

(*Functory related functions*) 
let () = Functory.Control.set_debug true (*On active le debug pour être sûr que Functory fonctionne *)
let hostname = Unix.gethostname ()


(*      Code de find_max, dont on ne se sert plus pour que l'IA soit plus rapide
let rec find_max p l =
	match l with
		|[] -> assert false
		|(m0,r0)::[] -> m0,r0
		|(m1,r1)::(m2,r2)::reste -> (match(compare p r1 r2) with 
												|Greater -> find_max p ((m2,r2)::reste) 
												|Smaller -> find_max p ((m1,r1)::reste)
												| _ -> find_max p ((m2,r2)::reste))
												*)
		
(*Cache creation*)
let memory = Hashtbl.create 10000

(*  Prototype de cache qui permet d'enlever les cas où l'IA pense perdre alors qu'elle peut gagner,  car elle a déjà stocké en cache qu'elle perdait :

	let cache f =
	fun arg ->
	if Hashtbl.mem memory arg then 
		match snd(f arg) = best_for (turn arg) with
		| true -> Hashtbl.replace memory arg (f arg) ; 
				Hashtbl.find memory arg 
		| false ->  Hashtbl.find memory arg
	else
	  begin
	    let res = f arg in
	    Hashtbl.add memory arg res ;
	    res
	  end *)
	  
(*Asks the cache and puts data into *)	  
let cache f =
	fun arg ->
	if Hashtbl.mem memory arg then Hashtbl.find memory arg
	else
	  begin
	    let res = f arg in
	    Hashtbl.add memory arg res ;
	    res
	  end

(* Returns the first best possible move *)
let rec best_move state =
	match (result state) with 
		| Some (p) -> (None,p) (* When the game is over *)
		| None -> (* else *)
		let rec didi l = 
	  		match l with
				| [] -> assert false (*When there is no more valid moves,  displays an error *)
		 		| m :: [] -> (Some m, snd( cache best_move (play state m))) (* When there is just only one move to play, we automatically play it*)
		 		| m :: reste -> if (snd (cache best_move (play state m))) = best_for (turn state) then (Some m, best_for (turn state)) else (didi reste) (* As soon as it reachs a state where the player wins, it send the actual move to get to this state*)
    	in 
    		didi (List.filter (is_valid state) (all_moves state))
    		
    		
 (* Map function (for the map_fold call) *) 
let compute state move =
    match (result state) with 
        |Some x ->  assert false 
        |None -> (match ((snd (cache best_move (play state move))) = best_for (turn state)) with 
                  | true -> (Some move, best_for (turn state))
                  | false -> (Some move, snd(cache best_move (play state move))))

(* Map reduce function *) 
(*compare_fold function is in game.ml file *)
let tibo state =  
	let moves_list = (List.filter (is_valid state) (all_moves state)) in 
	map_fold_ac  ~f:(compute state)   ~fold:compare_fold  (None, worst_for (turn state))  moves_list
