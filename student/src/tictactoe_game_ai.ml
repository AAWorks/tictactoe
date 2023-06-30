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

let rec _s_check_opp_line
  (to_win : int)
  (dir : Position.t -> Position.t)
  (old_pos : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  (score : float)
  : float
  =
  let pos = dir old_pos in
  if to_win = 0
  then score
  else (
    match Map.find pieces pos with
    | Some new_piece ->
      if Piece.equal new_piece piece
      then
        _s_check_opp_line
          (to_win - 1)
          dir
          pos
          piece
          pieces
          game_kind
          (score +. 1.0)
      else score
    | None -> score)
;;

let rec _s_check_line
  (to_win : int)
  (dir : Position.t -> Position.t)
  (opp_dir : Position.t -> Position.t)
  (old_pos : Position.t)
  (og_pos : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  (score : float)
  : float
  =
  let pos = dir old_pos in
  if to_win = 0
  then score
  else (
    match Map.find pieces pos with
    | Some new_piece ->
      if Piece.equal new_piece piece
      then
        _s_check_line
          (to_win - 1)
          dir
          opp_dir
          pos
          og_pos
          piece
          pieces
          game_kind
          (score +. 1.0)
      else
        _s_check_opp_line to_win opp_dir og_pos piece pieces game_kind score
    | None ->
      _s_check_opp_line to_win opp_dir og_pos piece pieces game_kind score)
;;

let _s_check_cell
  (pos : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  : float
  =
  let to_win = Game_kind.win_length game_kind - 1 in
  let s_check_line_wrapper dir opp_dir : float =
    _s_check_line to_win dir opp_dir pos pos piece pieces game_kind 0.0
  in
  match
    List.max_elt
      [ s_check_line_wrapper Position.right Position.left
      ; s_check_line_wrapper Position.down Position.up
      ; s_check_line_wrapper Position.down_right Position.up_left
      ; s_check_line_wrapper Position.up_right Position.down_left
      ]
      ~compare:(fun v1 v2 -> Float.compare v1 v2)
  with
  | Some score -> score
  | None -> failwith "wtf"
;;

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  match evaluate ~game_kind ~pieces with
  | Game_over { winner = Some piece } ->
    if Piece.equal piece me then Float.infinity else Float.neg_infinity
  | _ -> 0.0
;;

let _ = score

(* let rec mimimax ~(me : Piece.t) ~(am_maximizing_player : bool) ~(depth :
   int) ~(pieces : Piece.t Position.Map.t) ~(game_kind : Game_kind.t) ~(alpha
   : float) ~(beta : float) : float = let node_score = score ~me ~game_kind
   ~pieces in match depth with | 0 -> node_score | _ -> (match Float.is_inf
   node_score with | true -> node_score | false -> let possible_moves =
   available_moves ~game_kind ~pieces in (match possible_moves with | [] ->
   node_score | _ :: _ -> let acc_function, init = match am_maximizing_player
   with | true -> Float.max, (-1.0 *. Float.infinity, alpha, beta) | false ->
   Float.min, (Float.infinity, alpha, beta) in let update_alpha_beta :
   value:float -> alpha:float -> beta:float -> float * float = match
   am_maximizing_player with | true -> fun ~value ~alpha ~beta -> Float.max
   alpha value, beta | false -> fun ~value ~alpha ~beta -> alpha, Float.min
   beta value in let should_cutoff : value:float -> alpha:float -> beta:float
   -> bool = match am_maximizing_player with | true -> fun ~value ~alpha:_
   ~beta -> Float.O.(value > beta) | false -> fun ~value ~alpha ~beta:_ ->
   Float.O.(value < alpha) in let out = List.fold_until possible_moves ~init
   ~finish:Tuple3.get1 ~f:(fun (acc_score, alpha, beta) possible_move -> let
   next_pieces = Map.set pieces ~key:possible_move ~data: (match
   am_maximizing_player with | true -> me | false -> Piece.flip me) in let
   score = acc_function acc_score (mimimax ~me ~am_maximizing_player:(not
   am_maximizing_player) ~depth:(depth - 1) ~pieces:next_pieces ~game_kind
   ~alpha ~beta) in match should_cutoff ~value:score ~alpha ~beta with | true
   -> Continue_or_stop.Stop score | false -> let alpha, beta =
   update_alpha_beta ~value:score ~alpha ~beta in Continue_or_stop.Continue
   (score, alpha, beta)) in out)) ;; *)

let rec minimax
  pos
  depth
  maximizingPlayer
  me
  pieces
  (game_state : Game_state.t)
  player
  avmoves
  : float
  =
  let game_kind = game_state.game_kind in
  let new_pieces = Map.set pieces ~key:pos ~data:me in
  let s = score ~me:player ~game_kind ~pieces:new_pieces in
  if depth = 0 || match s with 0.0 -> false | _ -> true
  then
    s
    (* print_endline "Score Check"; print_s [%message "" (new_pieces :
       Piece.t Position.Map.t)]; print_endline (Game_state.to_string_hum
       game_state); *)
  else if maximizingPlayer
  then (
    let new_avmoves = Set.remove avmoves pos in
    new_avmoves
    |> Set.map
         (module Float)
         ~f:(fun new_pos ->
           minimax
             new_pos
             (depth - 1)
             false
             (Piece.flip me)
             new_pieces
             game_state
             player
             new_avmoves)
    |> Set.fold ~init:Float.neg_infinity ~f:Float.max)
  else (
    let new_avmoves = Set.remove avmoves pos in
    new_avmoves
    |> Set.map
         (module Float)
         ~f:(fun new_pos ->
           minimax
             new_pos
             (depth - 1)
             true
             me
             new_pieces
             game_state
             player
             new_avmoves)
    |> Set.fold ~init:Float.infinity ~f:Float.min)
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
    |> List.map ~f:(fun pos ->
         ( minimax
             pos
             11
             true
             me
             pieces
             game_state
             me
             (Position.Set.of_list avmoves)
         , pos ))
    |> List.max_elt ~compare:(fun (_v1, _) (_v2, _) -> Float.compare _v1 _v2)
  with
  | Some (_v1, pos) ->
    print_endline (Game_state.to_string_hum game_state);
    (* print_s [%message "" (v1 : Float.t)]; *)
    pos
  | None -> random_move_strategy ~game_kind ~pieces
;;
