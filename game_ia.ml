open Game

(* Stupid IA: it take the first possible valid move. 
let rec best_move state =
  match List.filter (is_valid state) (all_moves state) with
    | [] -> assert false
    | m :: _ ->
        let player = turn state in
          (Some m, worst_for player)
        *)
let rec find_max p l =
	match l with
		|[] -> assert false
		|(m0,r0)::[] -> m0,r0
		|(m1,r1)::(m2,r2)::reste -> (match(compare p r1 r2) with 
												|Greater -> find_max p ((m2,r2)::reste) 
												|Smaller -> find_max p ((m1,r1)::reste)
												| _ -> find_max p ((m2,r2)::reste))
		
let rec best_move state =
	match (result state) with 
		| Some (p) -> (None,p)
		| None ->
		let rec didi l = 
	  		match l with
		 		| [] -> []
		 		| m :: reste -> (Some m,( snd (best_move (play state m))))::(didi reste)
    	in 
    		let liste = trier_moves (all_moves state) state in 
    		find_max (turn state) (didi (List.filter (is_valid state) liste))
