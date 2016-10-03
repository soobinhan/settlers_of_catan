open Definition
open Registry
open Constant
open Util

let name = "init"

module Bot = functor (S: Soul) -> struct

  let initialize () = ()
  let i = ref 0

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

  let roll_probs roll =
    match roll with
    |6 |8 -> 5
    |5 |9 -> 4
    |4 |10 -> 3
    |3 |11 -> 2
    |2 |12 -> 1
    |0 -> 0
    |_ -> failwith "this shouldn't happen yo"
  

  let viable_settle g p=
    let overlap = List.nth g.ilist p <> None in
    let adj_list = adjacent_points p in
    let adjacent = List.fold_left
      (fun acc x -> acc || List.nth g.ilist x <> None) false adj_list
    in
    not (overlap || adjacent)

  let first_round s =  
    let (b,_,_,(color,_)) = s in
    let ((hex,port),(ilist,rlist),_,_,_) = b in
    let find_sixeight (i,acc) (_,r) =
      if r = 6 || r = 8 then
        (i+1,i::acc)
      else (i+1,acc)
    in
    let six_and_eight = snd(List.fold_left find_sixeight (0,[]) hex) in
    let fold' acc e =
      let fold'' acc' e' =
        if viable_settle (game_of_state s) e' then e'::acc'
        else acc'
      in
      List.fold_left fold'' acc (piece_corners e)
    in
    let calc_roll_sum p =
      let fold_h acc e =
        acc + roll_probs (snd (List.nth hex e))
      in
      List.fold_left fold_h 0 (adjacent_pieces p)
    in
    let find_max (p,sum) e =
      if (calc_roll_sum e) > sum then
        (e,(calc_roll_sum e))
      else (p,sum)
    in
    let viable_pts = List.fold_left fold' [] six_and_eight in
    let settle_pt = fst(List.fold_left find_max (0,0) viable_pts) in
    let road_pt = fst(List.fold_left find_max (0,0) (adjacent_points settle_pt)) in
    (settle_pt,road_pt)

  let second_round s = 
    let (b,_,_,(color,_)) = s in
    let ((hex,port),(ilist,rlist),_,_,_) = b in
    let find' (i,acc) e =
      match e with
      |None -> (i+1,acc)
      |Some(c,_) -> if c = color then (i+1,i) else (i+1,acc)
    in
    let prev_s = snd(List.fold_left find' (0,0) ilist) in 
    let get_res acc i =
      let (t,_) = List.nth hex i in 
      if (not(List.mem t acc)) then t::acc
      else acc
    in
    let cur_terrains = List.fold_left get_res [] (adjacent_pieces prev_s) in
    let fold' (i,acc) (t,r) =
      if not (List.mem t cur_terrains) then (i+1,(i,r)::acc) 
      else (i+1,acc)
    in
    let dont_have = snd(List.fold_left fold' (0,[]) hex) in
    let find_max (c_i,c_max) (i,r) =
      if (roll_probs r) > c_max then (i,(roll_probs r))
      else (c_i,c_max)
    in
    let calc_roll_sum p =
      let fold_h acc e =
        acc + roll_probs (snd (List.nth hex e))
      in
      List.fold_left fold_h 0 (adjacent_pieces p)
    in
    let find_max2 (p,sum) e =
      if (calc_roll_sum e) > sum then
        (e,(calc_roll_sum e))
      else (p,sum)
    in
    let max = fst(List.fold_left find_max (0,0) dont_have) in
    let rec find_settle_pt p lst=
      let fold' acc e =
        if viable_settle (game_of_state s) e then e::acc
        else acc
      in
      let v_pts = List.fold_left fold' [] (piece_corners p) in
      match v_pts with
      |[] -> 
        let new_dhve = list_memremove(fun (i,_) -> i = p) lst in
        let new_max = fst(List.fold_left find_max (0,0) new_dhve) in
        find_settle_pt new_max new_dhve 
      |l -> fst(List.fold_left find_max2 (0,0) l) 
    in
    let settle_pt = find_settle_pt max dont_have in
    let road_pt = fst(List.fold_left find_max2 (0,0) (adjacent_points settle_pt)) in
    (settle_pt,road_pt)
        
  let optimal_initial s =
    let (b,_,_,(color,_)) = s in
    let ((hex,port),(ilist,rlist),_,_,_) = b in
     let find' acc e =
      match e with
      |None -> acc
      |Some(c,_) -> if c = color then e::acc else acc
    in
    let c_settle = List.fold_left find' [] ilist in
    match c_settle with
    |[] -> first_round s
    |_ -> second_round s
    
  let handle_request (s: state) : move = 
    let (board, plist, turn, next) = s in
    let (c, req) = next in 
    match req with 
    | InitialRequest -> InitialMove(optimal_initial s)
    | RobberRequest -> InitialMove(optimal_initial s)
    | DiscardRequest-> InitialMove(optimal_initial s)
    | TradeRequest -> InitialMove(optimal_initial s)
    | ActionRequest -> InitialMove(optimal_initial s) 
end

(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
