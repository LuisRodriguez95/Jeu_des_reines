open Gamebase
open Game
open Functory.Network
open Functory.Network.Same

let ask_move state =
  Printf.printf "  => A vous de jouer ! \n%!" ;  
  let line = drag_n_drop state in (*Drag and drop to catch the move *)


  match readmove line with
  | None ->
    Printf.printf "\n Impossible de lire ce mouvement : %s\n\n%!" line ;
    None
    
  | Some mov ->
    if not (is_valid state mov) then
      begin
        Printf.printf "\n Ce mouvement est invalide : %s\n\n%!" (move2s mov) ;
        None
      end
    else Some mov

(* Get the move from the IA. *)
let ia_move state =
  let (mov, _) = Game_ia.tibo state in
  match mov with
  | None -> assert false
  | Some m -> m
 
	

(*** Each player in turn. ***)
    
let rec run with_ia state =
  (* Print state & which player to play. *)
  (*Printf.printf "\n%s\n %s to play.\n\n%!" (state2s state) (player2s (turn state)) ;*)
  	state_to_graphic state;
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

let presentation () = Printf.printf "\nBonjour et bienvenue sur le jeu des reines !\n" ;
				Printf.printf "Vous contrôlez des pions et face à vous se trouve une redoutable dame...\n" ;
				Printf.printf "Votre rôle sera de déplacer les pions jusqu'à ce qu'un de ceux-ci arrive de l'autre côté du plateau ou tue la dame.\n\n" ;
				Printf.printf "Pour déplacer un pion, vous devez le sélectionner avec le clic gauche de votre souris puis relâcher le clic sur la case de votre choix.\n" ;
				Printf.printf "En vert, s'affichent les déplacements possibles lorsque vous avez sélectionné un pion.\n" ;
				Printf.printf "Bonne chance !\n" 

let () = 
 (* Sys.argv are the command-line arguments. *)
  match Sys.argv with

  (* If there is one argument equal to "master" *)
  	| [| _ ; "master" |] -> 
     		Printf.printf "I am the master.\n%!" ;
     		declare_workers ~n:2 "localhost" ;
		graphic_initial ();
		presentation () ;
	  	run true initial
	 | _ -> 
    		 Printf.printf "I am a worker.\n%!" ;
     		Functory.Network.Same.Worker.compute ()
