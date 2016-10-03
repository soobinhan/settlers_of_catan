open Definition
open Registry
open Constant
open Util

let name = "road"

module Bot = functor (S: Soul) -> struct
    
  let number = ref 0

  let initialize () = number:=0

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

  let is_road_occupied rl (p1,p2) =
    let fold' acc (_,(p,p')) = acc || (p1 = p && p2 = p') || (p1 = p' && p2 = p) in
    List.fold_left fold' false rl
      
  let viable_line (b,p,t,n) = 
    let my_c = fst n in
    let(_,(_,rl),_,_,_) = b in
    let fold' acc (c,(p1,p2)) =
      let p1_exists = List.mem p1 acc in
      let p2_exists = List.mem p2 acc in
      if c = my_c then begin 
        if p1_exists && p2_exists then acc
        else if p1_exists && (not p2_exists) then p2::acc
        else if (not p1_exists) && p2_exists then p1::acc
        else p1::p2::acc
      end
      else acc
    in
    let my_rl = List.fold_left fold' [] rl in
    let create_possible acc p =
      let lines acc' p' =
        if not (is_road_occupied rl (p,p')) then (p,p')::acc'
        else acc'
      in
      List.fold_left lines acc (adjacent_points p)
    in
    let possible_lines = List.fold_left create_possible [] my_rl in
    pick_random possible_lines

  let viable_point (state: state) : point option =
    let (b, p, t, n) = state in
    let viable_settle g p=
      let overlap = List.nth g.ilist p <> None in
      let adj_list = adjacent_points p in
      let adjacent = List.fold_left 
	(fun acc x -> acc || List.nth g.ilist x <> None) false adj_list
      in
      not (overlap || adjacent)
    in 
    let linked g col p =
      let road' acc (c,(p1,p2)) = acc || (c=col && (p = p1 || p = p2)) in
      let is_linked_to_road = List.fold_left road' false g.rlist in
      match List.nth g.ilist p with
      |None -> is_linked_to_road || false
      |Some(c,_) -> is_linked_to_road || (c = col)
    in
    let viable_lst = 
      let rec helper x acc= 
	let game = game_of_state state in
	if x < 54 then
	  if (linked game (fst n) x) && viable_settle game x
	  then helper (x+1) (x::acc)
	  else helper (x+1) acc
	else acc
      in 
      helper 0 []
    in
    pick_random viable_lst

  let handle_request ((b,p,t,n) : state) : move =
    let (c, r) = n in
    let state = (b, p, t, n) in
    let game = game_of_state state in
    match r with
    | InitialRequest -> InitialMove(0, 0)
    | RobberRequest -> RobberMove(0, None)
    | DiscardRequest-> DiscardMove(0,0,0,0,0)
    | TradeRequest -> TradeResponse(true)
    | ActionRequest -> 
      if is_none t.dicerolled then Action(RollDice) 
      else 
        let enough_resources = 
          let cur_color = game.next_col in
          let cur_player_inv = 
            List.fold_left 
              (fun acc x -> if x.color = cur_color then x.inv else acc)
              (0, 0, 0, 0, 0) game.plist
          in
          let is_valid_cost current_inv cost = 
            (map_cost2 (fun e e' -> e>= e') current_inv cost) = (true,true,true,true,true)
          in
          is_valid_cost cur_player_inv cCOST_TOWN
        in      
        if enough_resources then
          match viable_point state with
          |None -> begin
            match viable_line state with
            |None -> Action(EndTurn)
            |Some l -> Action(BuyBuild(BuildRoad(c,l)))
          end
          |Some p -> Action(BuyBuild(BuildTown(p)))
        else
          match viable_line (b,p,t,n) with
          |None -> Action(EndTurn)
          |Some l -> Action(BuyBuild(BuildRoad(c,l)))

end

(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
