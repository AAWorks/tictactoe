open! Core
open Tic_tac_toe_2023_common
open Tic_tac_toe_exercises_lib
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  List.random_element_exn (available_moves ~game_kind ~pieces)
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  match List.random_element (winning_moves ~me ~game_kind ~pieces) with
  | Some move -> move
  | None -> List.random_element_exn (available_moves ~game_kind ~pieces)
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let win_moves = winning_moves ~me ~game_kind ~pieces in
  if List.is_empty win_moves
  then (
    let lose_moves = losing_moves ~me ~game_kind ~pieces in
    if List.is_empty lose_moves
    then random_move_strategy ~game_kind ~pieces
    else List.random_element_exn lose_moves)
  else List.random_element_exn win_moves
;;

let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let check_cell_wrapper pos : bool =
    match Map.find pieces pos with
    | Some piece -> check_cell pos piece pieces game_kind
    | None -> false
  in
  match List.find (boardlist game_kind) ~f:check_cell_wrapper with
  | Some pos ->
    if Piece.equal me (Map.find_exn pieces pos)
    then Float.infinity
    else Float.neg_infinity
  | None -> 0.0
;;

let _ = score

let rec minimax
  pos
  depth
  maximizingPlayer
  me
  pieces
  (game_state : Game_state.t)
  : float
  =
  let game_kind = game_state.game_kind in
  let new_pieces = Map.set pieces ~key:pos ~data:me in
  if depth = 0
     ||
     match evaluate ~game_kind ~pieces:new_pieces with
     | Game_continues -> false
     | _ -> true
  then
    (* print_endline "Score Check"; print_s [%message "" (new_pieces :
       Piece.t Position.Map.t)]; print_endline (Game_state.to_string_hum
       game_state); *)
    score ~me ~game_kind ~pieces:new_pieces
  else if maximizingPlayer
  then (
    let avmoves = available_moves ~game_kind ~pieces:new_pieces in
    List.fold avmoves ~init:Float.neg_infinity ~f:(fun accum new_pos ->
      match
        List.max_elt
          [ accum
          ; minimax
              new_pos
              (depth - 1)
              false
              (Piece.flip me)
              new_pieces
              game_state
          ]
          ~compare:Float.compare
      with
      | Some pos -> pos
      | None -> accum))
  else (
    let avmoves = available_moves ~game_kind ~pieces:new_pieces in
    List.fold avmoves ~init:Float.infinity ~f:(fun accum new_pos ->
      match
        List.min_elt
          [ accum
          ; minimax
              new_pos
              (depth - 1)
              true
              (Piece.flip me)
              new_pieces
              game_state
          ]
          ~compare:Float.compare
      with
      | Some pos -> pos
      | None -> accum))
;;

(* let _super_minimax ~(me : Piece.t) ~(game_kind : Game_kind.t) ~(pieces :
   Piece.t Position.Map.t) : Position.t = let win_moves = winning_moves ~me
   ~game_kind ~pieces in if List.is_empty win_moves then ( let lose_moves =
   losing_moves ~me ~game_kind ~pieces in if List.is_empty lose_moves then (
   let avmoves = available_moves ~game_kind ~pieces in match avmoves |>
   List.map ~f:(fun pos -> minimax pos 5 true game_state, pos) |>
   List.max_elt ~compare:(fun (v1, _) (v2, _) -> Float.compare v1 v2) with |
   Some (_, pos) -> pos | None -> random_move_strategy ~game_kind ~pieces)
   else List.random_element_exn lose_moves) else List.random_element_exn
   win_moves ;; *)

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  let avmoves = available_moves ~game_kind ~pieces in
  match
    avmoves
    |> List.map ~f:(fun pos -> minimax pos 9 false me pieces game_state, pos)
    |> List.max_elt ~compare:(fun (v1, _) (v2, _) -> Float.compare v1 v2)
  with
  | Some (v1, pos) ->
    print_endline (Game_state.to_string_hum game_state);
    print_s [%message "" (v1 : Float.t)];
    pos
  | None -> random_move_strategy ~game_kind ~pieces
;;
