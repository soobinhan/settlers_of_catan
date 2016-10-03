open Definition
open Constant
open Util
open Print
open Handlemove

type game = Handlemove.game
type player_rec = Handlemove.player_rec

let state_of_game g = Handlemove.state_of_game g

let game_of_state s = Handlemove.game_of_state s

let handle_move g m =
  let (cur_color,cur_player,_) = create_current_info g in
  let request = g.next_req in
  let c_rolldice = g.turn.dicerolled in
  match m with
  |InitialMove (p1,p2) -> begin
    match request with
    |InitialRequest ->
      if is_valid_point p1 && is_valid_point p2 then
        if viable_settle g p1 && are_points_adjacent p1 p2 then
          valid_init_move g (p1,p2)
        else
          invalid_init_move g
      else invalid_init_move g
    |RobberRequest -> invalid_rob_mov g
    |DiscardRequest -> invalid_discard_move g
    |TradeRequest -> trade_response g false
    |ActionRequest ->
      if is_none c_rolldice then roll_dice g
      else end_turn g
  end
  |RobberMove (piece, color) -> begin
    match request with
    |InitialRequest -> invalid_init_move g
    |RobberRequest ->
      if is_viable_robber_move g piece color then
        valid_rob_move g piece color
      else
        invalid_rob_mov g
    |DiscardRequest -> invalid_discard_move g
    |TradeRequest -> trade_response g false
    |ActionRequest ->
      if is_none c_rolldice then roll_dice g
      else end_turn g
  end
  |DiscardMove cost -> begin
    let (dis_color,_,dis_inv,dis_num,_) = create_discard_info g in
    match request with
    |InitialRequest -> invalid_init_move g
    |RobberRequest -> invalid_rob_mov g
    |DiscardRequest -> 
      if (sum_cost cost) = dis_num && is_valid_cost dis_inv cost then
        discard_move g cost
      else
        invalid_discard_move g
    |TradeRequest -> trade_response g false
    |ActionRequest ->
      if is_none c_rolldice then roll_dice g
      else end_turn g
  end
  |TradeResponse b -> begin
    match request with
    |InitialRequest -> invalid_init_move g
    |RobberRequest -> invalid_rob_mov g
    |DiscardRequest -> invalid_discard_move g
    |TradeRequest -> trade_response g b
    |ActionRequest ->
      if is_none c_rolldice then roll_dice g
      else end_turn g
  end
  |Action a -> begin
    match request with
    |InitialRequest -> invalid_init_move g
    |RobberRequest -> invalid_rob_mov g
    |DiscardRequest -> invalid_discard_move g
    |TradeRequest -> trade_response g false
    |ActionRequest -> begin
      match a with
      |RollDice ->
        roll_dice g
      |MaritimeTrade (sold,bought) ->
        if is_none c_rolldice then roll_dice g
        else maritime_trade g sold bought
      |DomesticTrade trade ->
        if is_none c_rolldice then roll_dice g
        else domestic_trade g trade
      |BuyBuild b -> begin
        match b with
        |BuildRoad (c,(p1,p2)) ->
          if is_none c_rolldice then roll_dice g
          else if is_valid_point p1 && is_valid_point p2 &&
              count_road g cur_color < cMAX_ROADS_PER_PLAYER then
            build_road g c (p1,p2)
          else end_turn g
        |BuildTown p ->
          if is_none c_rolldice then roll_dice g
          else if is_valid_point p &&
              count_settle g cur_color Town < cMAX_TOWNS_PER_PLAYER
          then build_town g p
          else end_turn g
        |BuildCity p ->
          if is_none c_rolldice then roll_dice g
          else if is_valid_point p &&
              count_settle g cur_color City < cMAX_CITIES_PER_PLAYER
          then build_city g p
          else end_turn g
        |BuildCard ->
          if is_none c_rolldice then roll_dice g
          else build_card g
      end
      |PlayCard pc ->
        if g.turn.cardplayed then end_turn g
        else
          let c_cards =
            match cur_player.cards with
            |Reveal l -> l
            |_ -> failwith "why is this happening"
          in
          begin
            match pc with
            |PlayKnight (piece,color) ->
              play_knight g piece color c_cards
            |PlayRoadBuilding (road, roadop) ->
              let _,(p1,p2) = road in
              if is_valid_point p1 && is_valid_point p2 then
                play_road_building g road roadop c_cards
              else end_turn g
            |PlayYearOfPlenty (res, resop) ->
              play_year_of_plenty g res resop c_cards
            |PlayMonopoly res ->
              play_monopoly g res c_cards
          end
      |EndTurn -> end_turn g
    end
  end
    
let init_game () = game_of_state (gen_initial_state())
  
let presentation g = 
   let color = g.next_col in
   let rec fold' plist acc = 
     match plist with 
     | h::t ->
       let player_hidden = {h with cards = hide h.cards} in
       if h.color <> color 
       then fold' t (player_hidden::acc)
       else fold' t (h::acc)
     | [] -> acc
   in
   let plist = g.plist in 
   {g with plist = List.rev (fold' plist [])}
