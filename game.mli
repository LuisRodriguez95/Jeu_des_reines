open Gamebase

(*** Abstract representation of a two-player game.
 *
 * The game can be tic-tac-toe, connect 4, chess, whatever. 
 *
*)
(* Type that defines the different pieces : Pawns and the Queen*)
type piece = Queen | Pawn of int | Empty

(* Type to define a direction*)
type dir = N | NE | E | SE | S | SO | O | NO 

(* The type 'state' represents a game configuration.
 *  for example, for tic-tac-toe it is a 3x3 grid 
 *  for connect 4 (puissance 4), it is a 6x7 grid 
 *
 * It contains also the current turn (i.e. which player can play now). *)
type state


(* This type represents a move. 
 *   tic-tac-toe: a coordinate in the grid.
 *   connect 4: a column number. *)
type move = piece * dir * int


(* This type represents the final result of a game.
 *   For example, it can be the final score of each player (two ints)
 *   or a value of type player indicating the winner (Human wins / Comput wins)
 *   or a three-value result: (Human wins / Comput wins / Even game) *)
type result


(* to_string functions *)
val state2s:   state -> string
val move2s:     move -> string
val result2s: result -> string
val piece2s: piece -> string



(*graphic functions *)
val state_to_graphic: state -> unit
val graphic_initial: unit -> unit
val drag_n_drop: state -> string

(* Reads a move from a string. 
 * Returns None if the string fails to be parsed. *)
val readmove: string -> move option

(* Applies a movemet from a coordonate depending on the direction and the distance *)
val movement: int*int -> dir -> int -> int * int

(* Returns true if a pawn has reached the top of the board*)
val pawn_win : piece matrix -> int -> bool

(* Initial state, when the game starts. *)
val initial: state

(* Indicates which player must play now. *)
val turn: state -> player

(* Indicates if a move is valid in the current state. *)
val is_valid: state -> move -> bool

(* Play a move *)
val play: state -> move -> state

(* Returns the list of moves that can be played in the current state, or any superset of it.
 * e.g. in connect 4, it can be a list of all columns (although not all columns are playable). 
 * Moves will be filtered by is_valid anyway. *)
val all_moves: state -> move list 

(* Returns the result of the game. None if the result is not known yet. *)
val result: state -> result option

(* Type used in the result of compare. See below. *)
type comparison = Equal | Greater | Smaller

(*   compare player old_result new_result:
 *
 *   compare player r1 r2  returns Equal if r1 and r2 are equivalent.
 *                         returns Greater if r2 is better than r1 (from the point of view of player). 
 *                         returns Smaller if r1 is better than r2 (from the point of view of player). *)
(*val compare: player -> result -> result -> comparison *)

val compare_fold: 'a * result -> 'a * result -> 'a * result

(* Returns the worst possible score for the given player. Useful for computing min or max.
 * The worst for H is supposed to be the best for C, and conversely. *)
val worst_for: player -> result

val best_for: player -> result
  
