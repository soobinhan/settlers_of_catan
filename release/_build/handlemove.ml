open Definition
open Constant
open Util
open Print

type player_rec = {
  color: color;
  inv: inventory;
  cards: cards;
  knights: knights;
  lroad: longestroad;
  larmy: largestarmy;
}

type game = {
  (*board*)
  (*map*)
  hex_list: hex list;
  port_list: port list;
  (*structures*)
  ilist: intersection list;
  rlist: road list;
  deck: deck;
  discard: discard;
  robber: robber;
  (*players*)
  plist : player_rec list;
  (*turn*)
  turn : turn;
  (*next*)
  next_col: color;
  next_req: request;
}

let state_of_game g = 
  let create_player p_rec =
    (p_rec.color, (p_rec.inv, p_rec.cards), 
     (p_rec.knights, p_rec.lroad, p_rec.larmy))
  in
  let map = g.hex_list, g.port_list in
  let board = (map, (g.ilist,g.rlist),g.deck,g.discard, g.robber) in
  let playerlist = List.map create_player g.plist in
  let next = (g.next_col, g.next_req) in
  (board,playerlist,g.turn,next)

let game_of_state s =
  (* converts player into playerRec *)
  let create_playerRec (p: player) : player_rec=
    let (color, (inv, cards), (knights, lroad, larmy)) = p in
    {color = color;
     inv = inv;
     cards = cards;
     knights = knights;
     lroad = lroad;
     larmy = larmy
    }
  in 
  let ((map, (ilist,rlist), deck, discard, robber), players, turn, next) = s in   
  {
    hex_list =  fst(map);
    port_list =  snd(map);
    ilist = ilist;
    rlist = rlist;
    deck = deck;
    discard = discard;
    robber = robber;
    plist = List.map create_playerRec players;
    turn = turn;
    next_col = fst(next);
    next_req = snd(next);
  }

(*************************HELPER FUNCTIONS*****************************)

let create_new_list new_e i lst = 
  let fold' e (index, acc) =
    if index = i then (index-1, new_e::acc)
    else (index-1, e::acc)
  in
  let (_,new_list) = List.fold_right fold' lst ((List.length lst)-1,[]) in
  new_list

let viable_settle g p=
  let overlap = List.nth g.ilist p <> None in
  let adj_list = adjacent_points p in
  let adjacent = List.fold_left 
    (fun acc x -> acc || List.nth g.ilist x <> None) false adj_list
  in
  not (overlap || adjacent)

let rec pick_cards i inv acc =
  let (b,w,o,g,l) = inv in
  let res_list = [(1,0,0,0,0);(0,1,0,0,0);(0,0,1,0,0);(0,0,0,1,0);(0,0,0,0,1)] in
  let inv_list = [b;w;o;g;l] in
  if (i = 0) || (sum_cost inv = 0) then acc
  else 
    let picked = weighted_random res_list inv_list in
    let new_inv = map_cost2 (-) inv picked in
    let new_acc = map_cost2 (+) picked acc in
    pick_cards (i-1) new_inv new_acc

let is_valid_cost current_inv cost = 
  (map_cost2 (fun e e' -> e>= e') current_inv cost) = (true,true,true,true,true)

let create_viable_victims g piece =
  let corners = piece_corners piece in
  let fold' acc e =
    match List.nth g.ilist e with
    |None -> acc
    |Some(c,_) -> 
      if (not (List.mem c acc)) && (c <> g.turn.active) then c::acc
      else acc
  in
  List.fold_left fold' [] corners

let are_points_adjacent p1 p2 =
  List.mem p1 (adjacent_points p2)

let is_road_occupied rl (p1,p2) =
  let fold' acc (_,(p,p')) = acc || (p1 = p && p2 = p') || (p1 = p' && p2 = p) in
  List.fold_left fold' false rl

let linked g col p =
  let road' acc (c,(p1,p2)) = acc || (c=col && (p = p1 || p = p2)) in
  let is_linked_to_road = List.fold_left road' false g.rlist in
  match List.nth g.ilist p with
  |None -> is_linked_to_road || false
  |Some(c,_) -> is_linked_to_road || (c = col)

let is_viable_robber_move g piece color =
  if piece > cMAX_PIECE_NUM || piece < cMIN_PIECE_NUM || piece = g.robber then
    false
  else
    let victims = create_viable_victims g piece in
    match victims,color with
    |[],None -> true
    |l, Some c -> List.mem c victims && g.turn.active <> c
    |_ -> false

let count_settle g color settle=
  let count' acc e =
    match e with
    |None -> acc
    |Some (c,s) -> 
      if c = color && s = settle then acc +1
      else acc
  in
  List.fold_left count' 0 g.ilist

let count_road g color =
  List.fold_left 
    (fun acc (c,_) -> if c = color then acc+1 else acc) 0 g.rlist

let is_valid_point p =
  cMIN_POINT_NUM <= p && p <= cMAX_POINT_NUM

let create_current_info g =
  let cur_color = g.turn.active in
  let cur_player = List.find(fun e -> cur_color = e.color) g.plist in
  let cur_index = list_indexof (fun e -> cur_color = e.color) g.plist in
  (cur_color,cur_player,cur_index)

let give_init_res g p =
  let (cur_color,cur_player,c_i) = create_current_info g in
  let fold' acc i =
    let (t,_) = List.nth g.hex_list i in
    if t <> Desert then t::acc
    else acc
  in
  let terrains = List.fold_left fold' [] (adjacent_pieces p) in
  let add acc e =
    map_cost2 (+) acc (single_resource_cost(get_some (resource_of_terrain e)))
  in
  let new_inv = List.fold_left add cur_player.inv terrains in
  let new_p = {cur_player with inv = new_inv} in
  create_new_list new_p c_i g.plist

let create_next_init g (p1,p2) =
  let c_color = g.turn.active in
  let next_color = next_turn c_color in
  let prev_color = prev_turn c_color in
  let next_turn = new_turn next_color in
  let prev_turn = new_turn prev_color in
  let cur_num_road = List.length g.rlist in
  if cur_num_road < 4 then
    let new_g = {g with turn = next_turn; next_col = next_color; 
      next_req = InitialRequest} in
    print_update c_color (InitialMove(p1,p2)) (state_of_game new_g);
    None,new_g
  else if cur_num_road = 4 then
    let g = g in
    print_update c_color (InitialMove(p1,p2)) (state_of_game g);
    None,g
  else if cur_num_road < 8 then
    let new_g = {g with turn = prev_turn; next_col = prev_color;
      next_req = InitialRequest; plist = (give_init_res g p1)} in
    print_update c_color (InitialMove(p1,p2)) (state_of_game new_g);
    None, new_g
  else 
    let new_g = {g with plist = (give_init_res g p1);next_req = ActionRequest} in
    print_update c_color (InitialMove(p1,p2)) (state_of_game new_g);
    None,new_g

let rec create_viable_piece g =
  let p = Random.int 19 in
  if p <> g.robber then p
  else create_viable_piece g

let rec create_next_discard g c =
  let player = List.find (fun e -> e.color = c) g.plist in
  let inv = player.inv in
  if c = g.turn.active then
    {g with next_col = g.turn.active; next_req = RobberRequest}
  else if sum_cost inv > cMAX_HAND_SIZE then
    {g with next_col = c; next_req = DiscardRequest}
  else create_next_discard g (next_turn c)

let create_discard_info g =
  let color = g.next_col in
  let player = List.find (fun e -> e.color = color) g.plist in
  let inv = player.inv in
  let num = (sum_cost inv)/2 in
  let index = list_indexof (fun e -> e.color = color) g.plist in
  (color,player,inv,num,index)

let l_army_update g =
  let (_,c_p,c_i) = create_current_info g in
  let c_knights = c_p.knights in
  let above_min = c_knights >= cMIN_LARGEST_ARMY in
  let fold' acc e = 
    if e.larmy then e::acc 
    else acc
  in
  let cur_larmy_p = List.fold_left fold' [] g.plist in
  match cur_larmy_p with
  |[] -> 
    if above_min then
      let new_c = {c_p with larmy = true} in
      let new_pl = create_new_list new_c c_i g.plist in
      {g with plist = new_pl}
    else g
  |h::_ -> 
    if c_knights > h.knights then
      let prev_i = list_indexof (fun e -> e = h) g.plist in
      let new_prev = {h with larmy = false} in
      let new_c = {c_p with larmy = true} in
      let pl1 = create_new_list new_c c_i g.plist in
      let pl2 = create_new_list new_prev prev_i pl1 in
      {g with plist = pl2}
    else g

let get_rlist g color =
  List.fold_left 
    (fun acc (c,l) -> if c = color then (c,l)::acc else acc) [] g.rlist

let l_road_update g =
  let (c,c_p,c_i) = create_current_info g in
  let il = g.ilist in
  let num_road = longest_road c (get_rlist g c) il in
  let fold' acc e =
    if e.lroad then e::acc
    else acc
  in
  let cur_lroad_p = List.fold_left fold' [] g.plist in
  let above_min = num_road >= cMIN_LONGEST_ROAD in
  match cur_lroad_p with
  |[] -> 
    if above_min then
      let new_c = {c_p with lroad = true} in
      let new_pl = create_new_list new_c c_i g.plist in
      {g with plist = new_pl}
    else g
  |h::_ -> 
    if num_road > (longest_road h.color (get_rlist g h.color) il) then
      let prev_i = list_indexof (fun e -> e = h) g.plist in
      let new_prev = {h with lroad = false} in
      let new_c = {c_p with lroad = true} in
      let pl1 = create_new_list new_c c_i g.plist in
      let pl2 = create_new_list new_prev prev_i pl1 in
      {g with plist = pl2}
    else g

let calculate_vp g =
  let (c_color,c_player,_) = create_current_info g in
  let count_vp_card acc e =
    if e = VictoryPoint then acc+1
    else acc
  in
  let num_vp_card = 
    match c_player.cards with
    |Reveal l -> List.fold_left count_vp_card 0 l
    |_ -> failwith "this shouldn't happen"
  in
  let num_town = count_settle g c_color Town in
  let num_city = count_settle g c_color City in
  num_town + num_vp_card + (2 * num_city) +
    (if c_player.larmy then 2 else 0) + (if c_player.lroad then 2 else 0)

let is_victory g = (calculate_vp g) = 10

(********************************************************************)

let end_turn g =
  let (c_c,c_p,c_i) = create_current_info g in
  let new_cards = 
    match g.turn.cardsbought,c_p.cards with
    |Reveal l, Reveal l2 -> l@l2
    |_ -> failwith "shouldn't happen"
  in
  let new_c = {c_p with cards = Reveal new_cards} in
  let new_pl = create_new_list new_c c_i g.plist in
  let next_color = next_turn (g.turn.active) in
  let final_game = {g with turn = new_turn next_color; next_col = next_color; 
    next_req = ActionRequest; plist = new_pl} in
  print_update g.turn.active (Action EndTurn) (state_of_game final_game);
  None,final_game

let valid_init_move g (p1,p2) =
  let (cur_color,cur_player,_) = create_current_info g in
  let new_ilist = create_new_list (Some (cur_color, Town)) p1 g.ilist in
  let new_g = {g with ilist = new_ilist; rlist = (cur_color, (p1,p2))::g.rlist} in
  create_next_init new_g (p1,p2)

let invalid_init_move g =
  let cur_color = g.turn.active in
  let rec rand_viable_line () =
    let rand_idx = Random.int 54 in
    let adj_idx = get_some (pick_random (adjacent_points rand_idx)) in
    if viable_settle g rand_idx then
      (rand_idx, adj_idx)
    else 
      rand_viable_line ()
  in
  let (r1, r2) = rand_viable_line () in
  let new_ilist = create_new_list (Some(cur_color, Town)) r1 g.ilist in
  let new_g = {g with ilist = new_ilist; rlist = (cur_color, (r1, r2))::g.rlist} in
  create_next_init new_g (r1,r2)

let valid_rob_move g piece color =
  let (cur_color,cur_player,cur_i) = create_current_info g in
  let robberupdated_game = {g with robber = piece; next_req = ActionRequest} in
  match color with
  |None -> 
    print_update cur_color (RobberMove (piece,color)) (state_of_game robberupdated_game);
    None, robberupdated_game 
  |Some c ->
    let victim = List.find(fun e -> c = e.color) g.plist in 
    let victim_i = list_indexof(fun e -> c = e.color) g.plist in
    let picked = pick_cards 1 victim.inv (0,0,0,0,0) in
    let new_vic_inv = map_cost2 (-) victim.inv picked in
    let new_current = 
      {cur_player with inv = map_cost2 (+) picked cur_player.inv}
    in
    let new_victim = {victim with inv = new_vic_inv} in
    let pl1 = create_new_list new_current cur_i g.plist in
    let new_plist = create_new_list new_victim victim_i pl1 in
    let final_g = {robberupdated_game with plist = new_plist} in
    print_update cur_color (RobberMove (piece,color)) (state_of_game final_g);
    None,final_g
    
let invalid_rob_mov g =
  let (cur_color,cur_player,cur_i) = create_current_info g in
  let viable_piece = create_viable_piece g in
  let robberupdated_game = {g with robber = viable_piece; next_req = ActionRequest} in
  let viable_victims = create_viable_victims g viable_piece in
  match pick_random viable_victims with
  |None ->
    print_update cur_color (RobberMove(viable_piece,None)) (state_of_game robberupdated_game);
    None, robberupdated_game
  |Some c -> 
    let victim = List.find(fun e -> c = e.color) g.plist in
    let victim_i = list_indexof(fun e -> c = e.color) g.plist in
    let picked = pick_cards 1 victim.inv (0,0,0,0,0) in
    let new_vic_inv = map_cost2 (-) victim.inv picked in
    let new_current = 
      {cur_player with inv = map_cost2 (+) picked cur_player.inv}
    in
    let new_victim = {victim with inv = new_vic_inv} in
    let pl1 = create_new_list new_current cur_i g.plist in
    let new_plist = create_new_list new_victim victim_i pl1 in
    let final_g = {robberupdated_game with plist = new_plist} in
    print_update cur_color (RobberMove(viable_piece,Some c)) (state_of_game final_g);
    None,final_g

let invalid_discard_move g =
  let (c,p,inv,num,p_i) = create_discard_info g in
  let picked = pick_cards num inv (0,0,0,0,0) in
  let new_inv = map_cost2 (-) inv picked in
  let new_d = {p with inv = new_inv} in
  let new_plist = create_new_list new_d p_i g.plist in
  let new_g = {g with plist = new_plist} in
  let final_g = create_next_discard new_g (next_turn c) in
  print_update c (DiscardMove picked) (state_of_game final_g);
  None,final_g

let discard_move g cost = 
  let (c,p,inv,num,p_i) = create_discard_info g in
  let new_inv = (map_cost2 (-) inv cost) in
  let new_d = {p with inv = new_inv} in
  let new_plist = create_new_list new_d p_i g.plist in
  let new_g = {g with plist = new_plist} in
  let final_g = create_next_discard new_g (next_turn c) in
  print_update c (DiscardMove cost) (state_of_game final_g);
  None,final_g
    
let trade_response g b =
  let (cur_color,cur_player,c_i) = create_current_info g in
  let trade = g.turn.pendingtrade in
  let new_turn = {g.turn with tradesmade = g.turn.tradesmade + 1; 
    pendingtrade = None} in
  let (color,cost1,cost2) = get_some trade in
  if b then
    let trader = List.find(fun e -> color = e.color) g.plist in
    let trader_i = list_indexof(fun e -> color = e.color) g.plist in
    let new_cur_inv = map_cost2 (+) cost2 (map_cost2 (-) cur_player.inv cost1) in
    let new_trader_inv = map_cost2 (+) cost1 (map_cost2 (-) trader.inv cost2) in 
    let new_current = {cur_player with inv = new_cur_inv} in 
    let new_trader = {trader with inv = new_trader_inv} in
    let pl1 = create_new_list new_current c_i g.plist in
    let new_plist = create_new_list new_trader trader_i pl1 in
    let new_g = {g with plist = new_plist; turn = new_turn;
      next_col = cur_color; next_req = ActionRequest} in
    print_update color (TradeResponse b) (state_of_game new_g);
    None,new_g
  else 
    let new_g = {g with turn = new_turn; next_col = cur_color; 
    next_req = ActionRequest} in
    print_update color (TradeResponse b) (state_of_game new_g);
    None,new_g

let roll_dice g =
  let roll = random_roll () in
  let hex_list = g.hex_list in
  let inter_list = g.ilist in
  let (cur_color,cur_player,_) = create_current_info g in
  if roll = cROBBER_ROLL then
    let new_turn = {g.turn with dicerolled = Some roll} in
    let new_g = {g with turn = new_turn} in
    let final_g =
      if (sum_cost cur_player.inv) > cMAX_HAND_SIZE then
        {new_g with next_req = DiscardRequest}
      else
      create_next_discard new_g (next_turn cur_color)
    in
    print_update g.turn.active (Action(RollDice)) (state_of_game final_g);
    None,final_g
  else
  (*finding hexes to updates and the corresponding intersections*)
  let fold' (i,acc) (terrain,roll') =
    if roll' = roll && (i <> g.robber) then 
      (i+1,(piece_corners i,terrain)::acc)
    else (i+1,acc)
  in
  let rolled_hexes = snd(List.fold_left fold' (0,[]) hex_list) in
  (*updating the player's inventories*)
  let updating plist1 (list,terrain) = 
    let fold' plist2 e =
      match List.nth inter_list e with
      |None -> plist2
      |Some(c,s) -> 
        let update = List.find (fun e -> c = e.color) plist2 in
        let update_i = list_indexof (fun e -> c = e.color) plist2 in
        let res_cost =
          single_resource_cost (get_some (resource_of_terrain terrain))
        in
        let cost = 
          if s = City then
            map_cost (fun i -> i*2) res_cost 
          else res_cost
        in
        let update_inv = map_cost2 (+) update.inv cost in
        let new_update = {update with inv = update_inv} in
        create_new_list new_update update_i plist2
    in
    List.fold_left fold' plist1 list
  in
  let updated_plist = List.fold_left updating g.plist rolled_hexes in
  let new_turn = {g.turn with dicerolled = Some roll} in
  let new_g = {g with plist = updated_plist; turn = new_turn; 
    next_req = ActionRequest} in
  print_update g.turn.active (Action(RollDice)) (state_of_game new_g);
  None,new_g
 
let maritime_trade g sold bought =
  let (cur_color,cur_player,c_i) = create_current_info g in
  let ilist = g.ilist in
  let port_list = g.port_list in
  (*find all the settlements the current player has*)
  let find_settle (i,acc) e =
    match e with
    |None -> (i+1,acc)
    |Some(color,_) -> 
      if cur_color = color then (i+1, i::acc)
      else (i+1,acc)
  in
  let settle_locations = snd(List.fold_left find_settle (0,[]) ilist) in
  (*find all the settlements of the player that is on a port*)
  let find_ports acc ((p1,p2),ratio,res) = 
    if List.mem p1 settle_locations || List.mem p2 settle_locations 
    then (ratio,res)::acc
    else acc
  in
  let port_settle = List.fold_left find_ports [] port_list in
  (*find the minimum ratio*)
  let find_min_ratio cur_min (ratio,res) =
    match res with
    |Any -> if ratio < cur_min then ratio else cur_min
    |PortResource pres -> 
      if pres = sold && ratio < cur_min then ratio
      else cur_min
  in
  let min_ratio = List.fold_left find_min_ratio 4 port_settle in
  (*make the trade*)
  let sold_cost = map_cost (fun a -> min_ratio*a) (single_resource_cost sold) in
  (*checking that the current player has the resources to make the trade*)
  begin
    if (is_valid_cost cur_player.inv sold_cost) && 
      (g.turn.tradesmade < cNUM_TRADES_PER_TURN) 
    then
      let sold_inv = (map_cost2 (-) cur_player.inv sold_cost) in
      let final_inv = map_cost2 (+) (single_resource_cost bought) sold_inv in
      let new_current = {cur_player with inv = final_inv} in
      let new_plist = create_new_list new_current c_i g.plist in
      let new_turn = {g.turn with tradesmade = g.turn.tradesmade +1} in
      let new_g = {g with plist = new_plist; turn = new_turn} in
      print_update cur_color (Action(MaritimeTrade(sold,bought))) (state_of_game new_g);
      None,new_g
    else end_turn g
  end
    
let domestic_trade g trade =
  let (cur_color,cur_player,_) = create_current_info g in
  let (color,cost1,cost2) = trade in
  let trader = List.find(fun e -> color = e.color) g.plist in
  if (is_valid_cost cur_player.inv cost1) && (is_valid_cost trader.inv cost2)
    && (cur_color <> color) && (g.turn.tradesmade < cNUM_TRADES_PER_TURN)
  then
    let new_turn = {g.turn with pendingtrade = Some trade;
      tradesmade = g.turn.tradesmade + 1} in
    let new_g = {g with turn = new_turn; next_col = color; next_req = TradeRequest} in
    print_update cur_color (Action(DomesticTrade trade)) (state_of_game new_g);
    None,new_g
  else
    end_turn g

let build_road g c l =
  let (cur_color,cur_player,c_i) = create_current_info g in
  let (p1,p2) = l in
  print_endline("( "^(string_of_int p1)^" ,"^(string_of_int p2)^" )");
  if c = cur_color && (not(is_road_occupied g.rlist l)) && 
    ((linked g c p1) || (linked g c p2)) && are_points_adjacent p1 p2 &&
    (is_valid_cost cur_player.inv cCOST_ROAD)
  then
    let new_i = map_cost2 (-) cur_player.inv cCOST_ROAD in
    let new_cur = {cur_player with inv = new_i} in
    let new_pl = create_new_list new_cur c_i g.plist in
    let new_g = {g with rlist = (c,l)::g.rlist; plist = new_pl} in
    let final_g = l_road_update new_g in
    print_update cur_color (Action(BuyBuild(BuildRoad(c,l)))) (state_of_game final_g);
    if is_victory final_g then
      (Some cur_color),final_g
    else
      None,final_g
  else
    end_turn g

let build_town g p =
  let (cur_color,cur_player,c_i) = create_current_info g in
  if viable_settle g p && linked g cur_color p 
    && is_valid_cost cur_player.inv cCOST_TOWN then
    let new_ilist = create_new_list (Some (cur_color,Town)) p g.ilist in
    let new_i = map_cost2 (-) cur_player.inv cCOST_TOWN in
    let new_cur = {cur_player with inv = new_i} in
    let new_pl = create_new_list new_cur c_i g.plist in
    let new_g = {g with ilist = new_ilist; plist = new_pl} in
    print_update cur_color (Action(BuyBuild(BuildTown p))) (state_of_game new_g);
    if is_victory new_g then
      (Some cur_color),new_g
    else
      None,new_g
  else
   end_turn g

let build_city g p =
  let (cur_color,cur_player,c_i) = create_current_info g in
  match List.nth g.ilist p with
  |Some(c,Town) ->
    if c = cur_color && (is_valid_cost cur_player.inv cCOST_CITY)
     then
      let new_ilist = 
        create_new_list (Some (cur_color, City)) p g.ilist 
      in
      let new_i = map_cost2 (-) cur_player.inv cCOST_CITY in
      let new_cur = {cur_player with inv = new_i} in
      let new_pl = create_new_list new_cur c_i g.plist in
      let new_g = {g with ilist = new_ilist; plist = new_pl} in
      print_update cur_color (Action(BuyBuild(BuildCity p))) (state_of_game new_g);
      if is_victory new_g then
        (Some cur_color),new_g
      else
        None,new_g
    else
      end_turn g
  |_ -> end_turn g

let build_card g = 
  let (cur_color,cur_player,c_i) = create_current_info g in
  if is_valid_cost cur_player.inv cCOST_CARD then
    match g.deck with
    |Reveal l -> begin
      match l with
      |[] -> end_turn g
      |_ -> 
        let (picked,new_deck) = pick_one l in
        let new_i = map_cost2 (-) cur_player.inv cCOST_CARD in
        let new_cb = 
          match g.turn.cardsbought with
          |Reveal c -> Reveal(picked::c)
          |_ -> failwith "shouldn't happen"
        in
        let new_turn = {g.turn with cardsbought = new_cb} in
        let new_player = {cur_player with inv = new_i} in
        let new_pl = create_new_list new_player c_i g.plist in
        let new_g = {g with deck = Reveal new_deck; plist = new_pl;turn = new_turn} in
        print_update cur_color (Action(BuyBuild(BuildCard))) (state_of_game new_g);
        if is_victory new_g then
          (Some cur_color),new_g
        else
          None,new_g
    end
    |_ -> failwith "this shouldn't happen"
  else end_turn g

(*I check if card is played in game.ml*)
let play_knight g piece color c_cards =
  let (cur_color,cur_player,i) = create_current_info g in
  if is_viable_robber_move g piece color && List.mem Knight c_cards then
    let new_cards = list_memremove (fun e -> e = Knight) c_cards in
    let new_c = {cur_player with knights = cur_player.knights+1; 
      cards = Reveal new_cards} in
    let new_pl = create_new_list new_c i g.plist in
    let new_turn = {g.turn with cardplayed = true} in
    (*I don't actually have to update next here.. right?*)
    let new_game = {g with turn = new_turn; robber = piece; 
      discard = Knight::g.discard; plist = new_pl} in
    let updated_game = l_army_update new_game in
    match color with
    |None ->
      print_update cur_color (Action(PlayCard(PlayKnight(piece,None)))) 
        (state_of_game updated_game);
      if is_victory updated_game then
        (Some cur_color),updated_game
      else
        None, updated_game
    |Some c ->
      let victim = List.find(fun e -> c = e.color) g.plist in 
      let v_i = list_indexof(fun e -> c = e.color) g.plist in
      let picked = pick_cards 1 victim.inv (0,0,0,0,0) in
      let new_vic_inv = map_cost2 (-) victim.inv picked in
      let new_c2 = 
        {new_c with inv = map_cost2 (+) picked new_c.inv}
      in
      let new_victim = {victim with inv = new_vic_inv} in
      let new_pl' = create_new_list new_c2 i g.plist in
      let new_pl2 = create_new_list new_victim v_i new_pl' in
      let new_g = {updated_game with plist = new_pl2} in
      print_update cur_color (Action(PlayCard(PlayKnight(piece,color)))) 
        (state_of_game new_g);
      if is_victory new_g then
        (Some cur_color),new_g
      else
        None,new_g
  else
    end_turn g

(*I check if card is played in game.ml*)
let play_road_building g road roadop c_cards = 
  let (cur_color,cur_player,i) = create_current_info g in
  let (c1,(p1,p1')) = road in
  if c1 = cur_color && (not(is_road_occupied g.rlist (p1,p1')))
  && List.mem RoadBuilding c_cards && (are_points_adjacent p1 p1') 
  && count_road g cur_color < cMAX_ROADS_PER_PLAYER then
    let new_cards = list_memremove (fun e -> e = RoadBuilding) c_cards in
    let new_c = {cur_player with cards = Reveal new_cards} in
    let new_pl = create_new_list new_c i g.plist in
    let new_turn = {g.turn with cardplayed = true} in
    let new_g = {g with rlist = (c1,(p1,p1'))::g.rlist; turn = new_turn;
                discard = RoadBuilding::g.discard; plist = new_pl} in
    match roadop with
    |None ->   
      let new_game = l_road_update new_g in
      print_update cur_color (Action(PlayCard(PlayRoadBuilding(road,roadop))))
        (state_of_game new_game);
      if is_victory new_game then
        (Some cur_color),new_game
      else
        None, new_g
    |Some (c2,(p2,p2')) ->
      if is_valid_point p2 && is_valid_point p2' then
        if c2 = cur_color && not(is_road_occupied new_g.rlist (p2,p2'))
        && are_points_adjacent p2 p2' 
        && count_road g cur_color < cMAX_ROADS_PER_PLAYER
        then
          let final_g = {new_g with rlist = (c2,(p2,p2'))::new_g.rlist} in
          print_update cur_color (Action(PlayCard(PlayRoadBuilding(road,roadop))))
            (state_of_game final_g);
          let fin_g = l_road_update final_g in
          if is_victory fin_g then
            (Some cur_color),fin_g
          else
            None,final_g
        else
          end_turn g
      else
        end_turn g
  else
    end_turn g

(*I check if card is played in game.ml*)
let play_year_of_plenty g res resop c_cards=
  let (cur_color,cur_player,i) = create_current_info g in
  if List.mem YearOfPlenty c_cards then
    let new_cards = list_memremove (fun e -> e = YearOfPlenty) c_cards in
    let new_inv = map_cost2 (+) cur_player.inv (single_resource_cost res) in
    let new_c = {cur_player with cards = Reveal new_cards; inv = new_inv} in
    let new_pl = create_new_list new_c i g.plist in
    let new_turn = {g.turn with cardplayed = true} in
    let new_game = {g with plist = new_pl; turn = new_turn;
                   discard = YearOfPlenty::g.discard} in
      match resop with
      |None ->
        print_update cur_color (Action(PlayCard(PlayYearOfPlenty(res,resop))))
          (state_of_game new_game);
        None, new_game
      |Some res' ->
        let inv2 = map_cost2 (+) new_inv (single_resource_cost res') in
        let new_c2 = {new_c with inv = inv2} in
        let new_pl2 = create_new_list new_c2 i g.plist in
        let new_g = {g with plist = new_pl2} in
        print_update cur_color (Action(PlayCard(PlayYearOfPlenty(res,resop))))
          (state_of_game new_g);
        None,new_g
  else
    end_turn g
      
(*I check if card is played in game.ml*)
let play_monopoly g res c_cards =
  let (cur_color,cur_player,i) = create_current_info g in
  if List.mem Monopoly c_cards then
    let get_resources acc p =
      acc + (num_resource_in_inventory p.inv res)
    in
    let num_res = List.fold_left get_resources 0 g.plist in
    let total_cost = map_cost (fun a -> a * num_res) (single_resource_cost res) in
    let update_inv = map_cost2 (+) cur_player.inv total_cost in
    let new_cards = list_memremove (fun e -> e = Monopoly) c_cards in
    let new_c = {cur_player with inv = update_inv; cards = Reveal new_cards} in
    let new_pl = create_new_list new_c i g.plist in
    let new_turn = {g.turn with cardplayed = true} in
    let new_g = {g with plist = new_pl; turn = new_turn; discard = Monopoly::g.discard} in
    print_update cur_color (Action(PlayCard(PlayMonopoly res))) (state_of_game new_g);
    None,new_g
  else
    end_turn g
