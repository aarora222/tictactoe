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
  let avl_positions =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  List.random_element_exn avl_positions
;;

let _ = random_move_strategy

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
  let avl_positions =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  let win_moves = winning_moves ~me ~game_kind ~pieces in
  match win_moves with
  | [] -> List.random_element_exn avl_positions
  | head :: _body -> head
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
  let opp_win_moves = winning_moves ~me:(Piece.flip me) ~game_kind ~pieces in
  let my_win_moves = winning_moves ~me ~game_kind ~pieces in
  let avl_moves = available_moves ~game_kind ~pieces in
  match my_win_moves with
  | [] ->
    (match opp_win_moves with
     | [] -> List.random_element_exn avl_moves
     | head :: _body -> head)
  | head :: _body -> head
;;

let _ = pick_winning_move_or_block_if_possible_strategy

(* disables unused warning. Feel free to delete once it's used. *)
let all_actions
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  =
  let avl_positions = available_moves ~game_kind ~pieces in
  let rec all_possible_positions
    ~(gs_list : Piece.t Position.Map.t list)
    ~(moves : Position.t list)
    =
    match moves with
    | [] -> gs_list
    | head :: body ->
      let new_map = Map.set pieces ~key:head ~data:me in
      let gs_list = gs_list @ [ new_map ] in
      all_possible_positions ~gs_list ~moves:body
  in
  all_possible_positions ~gs_list:[] ~moves:avl_positions
;;

let max a b =
  let com = Float.compare a b in
  if com > 0 then a else b
;;

let min a b =
  let com = Float.compare a b in
  if com < 0 then a else b
;;

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  =
  let _my_win_moves = winning_moves ~me ~game_kind ~pieces in
  let _opp_win_moves =
    winning_moves ~me:(Piece.flip me) ~game_kind ~pieces
  in
  (*let rec get_score ~(score : float) ~(moves : Position.t list) = match
    moves with | [] -> score | _head :: body -> let new_score = Float.add
    score 100.0 in get_score ~score:new_score ~moves:body in*)
  let state = evaluate ~game_kind ~pieces in
  match state with
  | Game_over { winner = Some winner } ->
    if Piece.equal winner me then Float.infinity else Float.neg_infinity
  | _ -> 0.0
;;

(*let my_score = get_score ~score:0.0 ~moves:my_win_moves in let opp_score =
  get_score ~score:0.0 ~moves:opp_win_moves in Float.sub my_score
  opp_score *)

let _ = score

let rec minimax
  ~(me : Piece.t)
  ~(maximizing_player : bool)
  ~(depth : int)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let avl_moves = available_moves ~game_kind ~pieces in
  let state_of_game = evaluate ~game_kind ~pieces in
  let game_ended =
    match state_of_game with
    | Evaluation.Game_over { winner = _ } -> true
    | _ -> false
  in
  let game_ended_or_mdepth = game_ended || depth = 0 in
  match game_ended_or_mdepth with
  | true -> score ~me ~game_kind ~pieces
  | false ->
    if maximizing_player
    then (
      let value = Float.neg_infinity in
      let rec get_max ~(avl_moves : Position.t list) ~(return_val : float) =
        match avl_moves with
        | [] -> return_val
        | head :: body ->
          let new_map = Map.set pieces ~key:head ~data:me in
          let comp_val =
            minimax
              ~me
              ~maximizing_player:false
              ~depth:(depth - 1)
              ~game_kind
              ~pieces:new_map
          in
          let m_val = max return_val comp_val in
          get_max ~avl_moves:body ~return_val:m_val
      in
      get_max ~avl_moves ~return_val:value)
    else (
      let value = Float.infinity in
      let rec get_min ~(avl_moves : Position.t list) ~(return_val : float) =
        match avl_moves with
        | [] -> return_val
        | head :: body ->
          let new_map = Map.set pieces ~key:head ~data:me in
          let comp_val =
            minimax
              ~me
              ~maximizing_player:true
              ~depth:(depth - 1)
              ~game_kind
              ~pieces:new_map
          in
          let m_val = min return_val comp_val in
          get_min ~avl_moves:body ~return_val:m_val
      in
      get_min ~avl_moves ~return_val:value)
;;

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
  let cur_score =
    score ~me ~game_kind:game_state.game_kind ~pieces:game_state.pieces
  in
  let avl_moves =
    available_moves ~game_kind:game_state.game_kind ~pieces:game_state.pieces
  in
  let all_pos_actions =
    all_actions ~me ~game_kind:game_state.game_kind ~pieces:game_state.pieces
  in
  let rec get_best_state
    ~(pos_actions : Piece.t Position.Map.t list)
    ~(state : Piece.t Position.Map.t)
    ~(sc : float)
    =
    match pos_actions with
    | [] -> state
    | head :: body ->
      let pot_score =
        minimax
          ~me
          ~maximizing_player:true
          ~depth:1
          ~game_kind:game_state.game_kind
          ~pieces:head
      in
      let m_score = max sc pot_score in
      let best_state = if Float.equal m_score sc then state else head in
      get_best_state ~pos_actions:body ~state:best_state ~sc:m_score
  in
  let best_state =
    get_best_state
      ~pos_actions:all_pos_actions
      ~state:game_state.pieces
      ~sc:cur_score
  in
  let rec get_position ~(avl_positions : Position.t list) : Position.t =
    match avl_positions with
    | [] -> List.random_element_exn avl_moves
    | head :: body ->
      let is_pos = Map.mem best_state head in
      if is_pos then head else get_position ~avl_positions:body
  in
  get_position ~avl_positions:avl_moves
;;
