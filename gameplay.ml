open Gamebase
open Game

(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)
let ask_move state =
  Printf.printf "  => Your move ? %!" ;  
  let line = read_line () in

  match readmove line with
  | None ->
    Printf.printf "\n Cannot read this move: %s\n\n%!" line ;
    None
    
  | Some mov ->
    if not (is_valid state mov) then
      begin
        Printf.printf "\n This move is invalid: %s\n\n%!" (move2s mov) ;
        None
      end
    else Some mov

(* Get the move from the IA. *)
let ia_move state =
  let (mov, _) = Game_ia.best_move state in
  match mov with
  | None -> assert false
  | Some m -> m
  
(*** Each player in turn. ***)
    
let rec run with_ia state =
  
  (* Print state & which player to play. *)
  Printf.printf "\n%s\n %s to play.\n\n%!" (state2s state) (player2s (turn state)) ;
  
  match result state with
  | Some r ->
    (* Game is finished. Print result. *)
    Printf.printf "*** %s ***\n%!" (result2s r) ;
    ()
    
  | None ->
    (* Game is not finished. Play one turn. *)

    let state' =
      if with_ia && turn state = Comput
      then play state (ia_move state)
      else
        begin match ask_move state with
          | None -> state (* Invalid move, play same state again. *)
          | Some mov -> play state mov
        end
    in
    run with_ia state'


let () = run true initial
