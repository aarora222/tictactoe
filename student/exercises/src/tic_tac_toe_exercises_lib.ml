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

let get_all_positions ~(game_kind : Game_kind.t) : Position.t list =
  let board_length = Game_kind.board_length game_kind in
  let positions =
    List.init board_length ~f:(fun x ->
      List.init board_length ~f:(fun y -> { Position.row = x; column = y }))
  in
  List.concat positions
;;

let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let all_positions = get_all_positions ~game_kind in
  let taken_positions = Map.keys pieces in
  let not_in_taken a =
    let bool = List.mem taken_positions a ~equal:Position.equal in
    match bool with true -> false | false -> true
  in
  List.filter all_positions ~f:not_in_taken
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

(*let position_list_down ~(game_kind: Game_kind.t) ~(position: Position.t) = 
   let rec populate_list ~(last_val: Position.t) n ~(list: Position.t list) =
    if (n = 0) then 
      (list)
    else (
      let new_pos = Position.down last_val in 
      let list = list @ [new_pos] in 
      populate_list ~last_val: new_pos (n-1) ~list: list
    )
  in  
  let l = [position] in 
  populate_list ~last_val: position (Game_kind.win_length game_kind) ~list: l
   
;;
let position_list_diag_right ~(game_kind: Game_kind.t) ~(position: Position.t) = 
  let rec populate_list ~(last_val: Position.t) n ~(list: Position.t list) =
   if (n = 0) then 
     (list)
   else (
     let new_pos = Position.right (Position.down last_val) in 
     let list = list @ [new_pos] in 
     populate_list ~last_val: new_pos (n-1) ~list: list
   )
 in  
 let l = [position] in 
 populate_list ~last_val: position (Game_kind.win_length game_kind) ~list: l
;;



let position_list_left ~(game_kind: Game_kind.t) ~(position: Position.t) = 
  let rec populate_list ~(last_val: Position.t) n ~(list: Position.t list) =
   if (n = 0) then 
     (list)
   else (
     let new_pos = Position.left last_val in 
     let list = list @ [new_pos] in 
     populate_list ~last_val: new_pos (n-1) ~list: list
   )
 in  
 let l = [position] in 
 populate_list ~last_val: position (Game_kind.win_length game_kind) ~list: l
  
;;
*)
let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let rec check_for_win ~(n: int) ~(pos: Position.t) ~(piece_type: Piece.t) 
  ~(direction: int)= 
    if (n = Game_kind.win_length game_kind) then
      true
    else (
      let p = 
      (match direction with
      | 1 -> Map.find pieces (Position.down pos) 
      | 2 -> Map.find pieces (Position.right pos)
      | 3 -> Map.find pieces (Position.down (Position.right pos)) 
      | 4 -> Map.find pieces (Position.up (Position.right pos))
      | _ -> None 
      ) in 
      match p with
      | None -> false
      | Some p -> (if(Piece.equal p piece_type) 
        then (check_for_win ~n:(n+1) ~pos: p ~piece_type: piece_type ~direction: direction) 
        else false)
    )
  

;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  ignore me;
  ignore game_kind;
  ignore pieces;
  failwith "Implement me!"
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  ignore me;
  ignore game_kind;
  ignore pieces;
  failwith "Implement me!"
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
    (((row 0) (column 1)) ((row 0) (column 2)) ((row 1)
   (column 1)) ((row 1) (column 2)) ((row 2) (column 1))) |}]
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
(* let%expect_test "evalulate_win_for_x" = print_endline (evaluate
   ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces |>
   Evaluation.to_string); [%expect {| (Win (X)) |}] ;;

   let%expect_test "evalulate_non_win" = print_endline (evaluate
   ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   Evaluation.to_string); [%expect {| Game_continues |}] ;; *)

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
(*let%expect_test "winning_move" = let positions = winning_moves
  ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
  [%sexp (positions : Position.t list)]; [%expect {| ((((row 1) (column 1))))
  |}]; let positions = winning_moves ~game_kind:non_win.game_kind
  ~pieces:non_win.pieces ~me:Piece.O in print_s [%sexp (positions :
  Position.t list)]; [%expect {| () |}] ;;*)

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
(*let%expect_test "print_losing" = let positions = losing_moves
  ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
  [%sexp (positions : Position.t list)]; [%expect {| () |}]; let positions =
  losing_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
  ~me:Piece.O in print_s [%sexp (positions : Position.t list)]; [%expect {|
  ((((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2)) ((row 2)
  (column 1)))) |}] ;;*)
