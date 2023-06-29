open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

let boardlist game_kind =
  let board_length = Game_kind.board_length game_kind in
  let twod_move_list =
    List.init board_length ~f:(fun row ->
      List.init board_length ~f:(fun col -> { Position.row; column = col }))
  in
  List.concat twod_move_list
;;

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  List.filter (boardlist game_kind) ~f:(fun pos -> not (Map.mem pieces pos))
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

let rec check_line
  (to_win : int)
  (dir : Position.t -> Position.t)
  (old_pos : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  : bool
  =
  let pos = dir old_pos in
  if to_win = 0
  then true
  else (
    match Map.find pieces pos with
    | Some new_piece ->
      if Piece.equal new_piece piece
      then check_line (to_win - 1) dir pos piece pieces game_kind
      else false
    | None -> false)
;;

let check_cell
  (position : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  : bool
  =
  let to_win = Game_kind.win_length game_kind - 1 in
  let check_line_wrapper dir : bool =
    check_line to_win dir position piece pieces game_kind
  in
  let checks = Position.[ right; down; down_right; up_right ] in
  List.exists checks ~f:check_line_wrapper
;;

let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let check_cell_wrapper pos : bool =
    match Map.find pieces pos with
    | Some piece -> check_cell pos piece pieces game_kind
    | None -> false
  in
  let newlist = List.filter (boardlist game_kind) ~f:check_cell_wrapper in
  let x =
    List.exists newlist ~f:(fun pos ->
      Piece.equal (Map.find_exn pieces pos) Piece.X)
  in
  let o =
    List.exists newlist ~f:(fun pos ->
      Piece.equal (Map.find_exn pieces pos) Piece.O)
  in
  match x, o with
  | true, true -> Illegal_state
  | true, false -> Game_over { winner = Some Piece.X }
  | false, true -> Game_over { winner = Some Piece.O }
  | false, false ->
    if List.is_empty (available_moves ~game_kind ~pieces)
    then Game_over { winner = None }
    else Game_continues
;;

(* Exercise 3. *)
let rec w_check_opp_line
  (to_win : int)
  (dir : Position.t -> Position.t)
  (old_pos : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  : bool
  =
  let pos = dir old_pos in
  if to_win = 0
  then true
  else (
    match Map.find pieces pos with
    | Some new_piece ->
      if Piece.equal new_piece piece
      then w_check_opp_line (to_win - 1) dir pos piece pieces game_kind
      else false
    | None -> false)
;;

let rec w_check_line
  (to_win : int)
  (dir : Position.t -> Position.t)
  (opp_dir : Position.t -> Position.t)
  (old_pos : Position.t)
  (og_pos : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  : bool
  =
  let pos = dir old_pos in
  if to_win = 0
  then true
  else (
    match Map.find pieces pos with
    | Some new_piece ->
      if Piece.equal new_piece piece
      then
        w_check_line
          (to_win - 1)
          dir
          opp_dir
          pos
          og_pos
          piece
          pieces
          game_kind
      else w_check_opp_line to_win opp_dir og_pos piece pieces game_kind
    | None -> w_check_opp_line to_win opp_dir og_pos piece pieces game_kind)
;;

let w_check_cell
  (pos : Position.t)
  (piece : Piece.t)
  (pieces : Piece.t Position.Map.t)
  (game_kind : Game_kind.t)
  : bool
  =
  let to_win = Game_kind.win_length game_kind - 1 in
  let w_check_line_wrapper dir opp_dir : bool =
    w_check_line to_win dir opp_dir pos pos piece pieces game_kind
  in
  w_check_line_wrapper Position.right Position.left
  || w_check_line_wrapper Position.down Position.up
  || w_check_line_wrapper Position.down_right Position.up_left
  || w_check_line_wrapper Position.up_right Position.down_left
;;

let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let moves = available_moves ~game_kind ~pieces in
  List.filter moves ~f:(fun pos -> w_check_cell pos me pieces game_kind)
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let opp = Piece.flip me in
  winning_moves ~me:opp ~game_kind ~pieces
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {| 
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
    ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
let%expect_test "evalulate_win_for_x" =
  print_endline
    (evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evalulate_non_win" =
  print_endline
    (evaluate ~game_kind:non_win.game_kind ~pieces:non_win.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 1) (column 1)))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {|
  (((row 1) (column 1))) |}]
;;

let empty_game_ttt =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let empty_game_omok =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Omok in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let win_for_x_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

let win_for_o_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
;;

let diag_win_for_o_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let invalid_state_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let tie_ttt =
  empty_game_ttt
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
;;

let win_for_x_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 4 }
;;

let non_win_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 13; column = 9 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 13; column = 8 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 13; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 12 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 10; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 9; column = 9 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 5; column = 8 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 6; column = 7 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 8; column = 12 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 18; column = 13 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 13; column = 14 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 15; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 7; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 11; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 12; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 8; column = 14 }
;;

let minor_diag_win_for_x_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 14; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 13; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 12; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 12; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 11; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 8 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 10; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 8 }
;;

let win_for_o_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 8 }
;;

let invalid_omok =
  empty_game_omok
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 4 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 3 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 5 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 4 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 6 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 7 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 5 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 8 }
;;

let%expect_test "evaluate_win_for_x_ttt" =
  print_endline
    (evaluate ~game_kind:win_for_x_ttt.game_kind ~pieces:win_for_x_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evaluate_non_win_ttt" =
  print_endline
    (evaluate ~game_kind:non_win_ttt.game_kind ~pieces:non_win_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

let%expect_test "evaluate_win_for_o_ttt" =
  print_endline
    (evaluate ~game_kind:win_for_o_ttt.game_kind ~pieces:win_for_o_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(O))) |}]
;;

let%expect_test "evaluate_tie_ttt" =
  print_endline
    (evaluate ~game_kind:tie_ttt.game_kind ~pieces:tie_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner())) |}]
;;

let%expect_test "evaluate_diag_win_o_ttt" =
  print_endline
    (evaluate
       ~game_kind:diag_win_for_o_ttt.game_kind
       ~pieces:diag_win_for_o_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(O))) |}]
;;

let%expect_test "evaluate_invalid_game_ttt" =
  print_endline
    (evaluate
       ~game_kind:invalid_state_ttt.game_kind
       ~pieces:invalid_state_ttt.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner())) |}]
;;

let%expect_test "evaluate_win_for_x_omok" =
  print_endline
    (evaluate
       ~game_kind:win_for_x_omok.game_kind
       ~pieces:win_for_x_omok.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evaluate_non_win_omok" =
  print_endline
    (evaluate ~game_kind:non_win_omok.game_kind ~pieces:non_win_omok.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

let%expect_test "evaluate_minor_diag_win_for_x_omok" =
  print_endline
    (evaluate
       ~game_kind:minor_diag_win_for_x_omok.game_kind
       ~pieces:minor_diag_win_for_x_omok.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evaluate_win_for_o_omok" =
  print_endline
    (evaluate
       ~game_kind:win_for_o_omok.game_kind
       ~pieces:win_for_o_omok.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(O))) |}]
;;

let%expect_test "evaluate_invalid_omok" =
  print_endline
    (evaluate ~game_kind:invalid_omok.game_kind ~pieces:invalid_omok.pieces
     |> Evaluation.to_string);
  [%expect {| Illegal_state |}]
;;

let%expect_test "winning_move_ttt" =
  let positions =
    winning_moves
      ~game_kind:non_win_ttt.game_kind
      ~pieces:non_win_ttt.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 1) (column 1)))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win_ttt.game_kind
      ~pieces:non_win_ttt.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

let%expect_test "winning_move_omok" =
  let positions =
    winning_moves
      ~game_kind:non_win_omok.game_kind
      ~pieces:non_win_omok.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| ()
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win_omok.game_kind
      ~pieces:non_win_omok.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 12) (column 6))) |}]
;;
