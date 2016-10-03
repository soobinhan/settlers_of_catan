open Definition
open Registry
open Constant
open Util
open Print

let name = "trade"

module Bot = functor (S: Soul) -> struct

  let initialize () = ()

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

   let rec memremove (p : 'a -> bool) (lst : 'a list) : 'a list =
    match lst with
      | [] -> []
      | h::t -> if p h then t else h::(memremove p t)
  

  let is_road_occupied rl (p1,p2) =
    let fold' acc (_,(p,p')) = acc || (p1 = p && p2 = p') || (p1 = p' && p2 = p) in
    List.fold_left fold' false rl
      
  let calculate_vp_color g color =
  let c_rec = (*player record of the given color*)
    let c_rec_wrapped =
      List.fold_left 
        (fun acc x -> if x.color = color then x::acc else acc)
        [] g.plist
      in
      match c_rec_wrapped with 
      |h::_ -> h
      |[] -> failwith "shouldn't happen!!!!!!!!! Zardoz warns."
    in
  let count_vp_card acc e =
    if e = VictoryPoint then acc+1
    else acc
  in
  let num_vp_card = 
    match c_rec.cards with
    |Reveal l -> List.fold_left count_vp_card 0 l
    |_ -> failwith "this shouldn't happen"
  in
  let count_settle g color settle=
    let count' acc e =
      match e with
      |None -> acc
      |Some (c,s) -> 
        if c = color && s = settle then acc +1
        else acc
    in
    List.fold_left count' 0 g.ilist
  in
  let num_town = count_settle g color Town in
  let num_city = count_settle g color City in
  num_town + num_vp_card + (2 * num_city) +
    (if c_rec.larmy then 2 else 0) + (if c_rec.lroad then 2 else 0)

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

  let num_trade = ref 0
  let players_w_res = ref []

  let handle_request ((b,p,t,n) : state) : move =
    let (c, r) = n in
    let state = (b, p, t, n) in
    let game = game_of_state state in
    let me = List.find (fun x -> x.color = c) game.plist in
    let my_inventory = me.inv in
    match r with
    | InitialRequest -> InitialMove(0, 0)
    | RobberRequest -> RobberMove(0, None)
    | DiscardRequest-> DiscardMove(0,0,0,0,0)
    | TradeRequest -> 
      let request = game.turn.pendingtrade in
      let (c_opponent, offered, requested) = get_some request in
      let sum_offered = sum_cost offered in
      let sum_requested = sum_cost requested in
      let is_good_ratio = (float_of_int sum_offered) /. (float_of_int sum_requested) >= 1. in
      let other_guy_winning = 
        (calculate_vp_color game c_opponent) >= 7
      in
      let lack (cst: cost) (inv: cost): bool = (*c1 is smaller than c2*)
        map_cost2 (fun i1 i2 -> if i1 > 0 then i2 > 3 else true) cst inv = (true, true, true, true, true)
      in
      let lack_requested = lack requested my_inventory in
      let lack_offered = lack offered my_inventory in
      if is_good_ratio then 
        if other_guy_winning then
          if not lack_offered then 
            if not lack_requested then 
              TradeResponse(true)
            else TradeResponse(false)
          else TradeResponse(false)
        else TradeResponse(false)
      else TradeResponse(false)

    | ActionRequest -> 
      if is_none t.dicerolled then Action(RollDice) 
      else 
        (*list of type of resources that i can't get i.e. terrains that i'm not adjacent to*)
        let potentially_lack_res = 
          (*list of all my settlements*)
          let settlement_list = 
            snd(List.fold_left
                  (fun (n, acc) x -> 
                   match x with 
                   |Some (clr, settlement) -> 
                      if clr = c then (n + 1, n::acc) else (n + 1, acc)
                   |None -> (n + 1, acc)
                  ) 
                  (0, []) 
                  game.ilist
                )
          in
          (*list of all hexes im adjacent to*)
          let piece_list =
            (*folds over the list of adjacent pieces and adds them*) 
            let fold' (acc: piece list) (point: point): piece list =
              List.fold_left
                (fun acc x -> if List.mem x acc then x::acc else acc)
                acc (adjacent_pieces point)  
            in
            List.fold_left (fun acc x -> fold' acc x) [] settlement_list
          in
          let hex_list = 
            List.fold_left
              (fun acc x-> (List.nth game.hex_list x)::acc) [] piece_list 
          in
          (*list of terrains i'm not adjacent to*)
          let not_adjacent_terrain = 
            let ter_list = [Hill;Pasture;Mountain;Field;Forest;Desert] in
            List.fold_left 
              (fun acc (ter, roll) -> (memremove (fun e -> e = ter) acc)) 
              ter_list 
              hex_list
          in
          (*list of resources that aren't available*)
          let not_avail_resource =
            List.fold_left 
              (fun acc x -> (resource_of_terrain x)::acc) 
              [] not_adjacent_terrain
          in
          not_avail_resource
        in(*end of potentially lacking resources*)

        (*currently lacking resource: type resource*)
        let lack_res = 
          let index_res = function
              |0 -> Brick
              |1 -> Wool
              |2 -> Ore
              |3 -> Grain
              |4 -> Lumber
              |_ -> failwith "no"
          in

          let lack l = 
            List.fold_left(fun acc e -> if e = 0 then acc+1 else acc) 0 l
          in

          let town =
            let y_call_town cost = 
              let (b,w,o,g,l) = cost in
                print_endline (string_of_list string_of_int [b;w;g;l]);
              if (lack [b;w;g;l]) <= 2 then
                let weird_fold (i,acc) e =
                  if i = 2 then (i+1,acc)
                  else 
                    if e = 0 then (i+1,i::acc)
                    else (i+1,acc)
                in
                let need_i = snd(List.fold_left weird_fold (0,[]) [b;w;o;g;l]) in
                let need_res = List.map index_res need_i in
                match need_res with
                |h::h1::_ -> (true,(h,h1))
                |h::_ -> (true,(h,h))
                |[] -> (false, (Brick,Brick))
              else (false,(Brick,Brick))
            in
            match y_call_town my_inventory with 
            |(true, (r1, r2)) -> [r1;r2]
            |_ -> []
          in 

          let pick_min c = 
            let (b, w, o, g, l) = c in 
            let inv_lst = [(Brick, b);(Wool, w);(Ore, o);(Grain, g);(Lumber, l)] in
            let min = 
              List.fold_left 
                (fun (acc_res, acc_n) (res, n) -> if n < acc_n then (res, n) else (acc_res, acc_n))
                (Brick, 0)
                inv_lst
            in
            min
          in
          match town with 
          |[r1; r2] -> begin
            let tuple =
              List.fold_left 
                (fun acc x -> (List.mem x potentially_lack_res)::acc) 
                [] 
                [Some r1; Some r2]
            in
            match tuple with
            |[true; _] -> r1
            |[_; true] -> r2
            |_ -> fst (pick_min my_inventory)
           end
          | _ -> fst (pick_min my_inventory)
        in (*end of lack_res*)

        (*not needed resources: resource option*)
        let abun_or_unnec_res = 
          let std = 3 in
          let pick_max c = 
            let (b, w, o, g, l) = c in 
            let inv_lst = [(Brick, b);(Wool, w);(Ore, o);(Grain, g);(Lumber, l)] in
            let max = 
              List.fold_left 
                (fun (acc_res, acc_n) (res, n) -> if n > acc_n then (res, n) else (acc_res, acc_n))
                (Brick, 0)
                inv_lst
            in
            max
          in
          match pick_max my_inventory with 
          | (res, number) -> if number >= std then Some res else None
        in

        let create_cost (res: resource) (n: int) : cost =
          match res with 
          |Brick -> (n, 0, 0, 0, 0)
          |Wool -> (0, n, 0, 0, 0)
          |Ore -> (0, 0, n, 0, 0)
          |Grain -> (0, 0, 0, n, 0)
          |Lumber -> (0, 0, 0, 0, n)
        in

        if !num_trade < 2 then
          let players_w_res_buffer = 
            if !num_trade = 0 then 
              let rec fold' (boolean: bool) (inv_lst: (resource * int) list) : bool  = 
                match inv_lst with
                |(hr, hn)::t -> 
                  if hr = lack_res then 
                  fold' ((hn > 0)||boolean) t 
                  else  fold' boolean t
                |[] -> boolean
              in
              (List.fold_left 
                (fun acc p -> 
                  let (b, w, o, g, l) = p.inv in
                  let inv_lst = [(Brick, b); (Wool, w); (Ore, o); (Grain, g); (Lumber, l)] in
                  if (fold' false inv_lst) then p.color::acc else acc
                ) [] game.plist)
            else
            !players_w_res
          in
          (*list of players(colors) with the resources that i lack*) 
          players_w_res := players_w_res_buffer;

          (*color of a random player that has at least one resource: color option*)
          let player_w_most = 
            let wrapped = 
                List.fold_left
                  (fun (n, acc) pl -> 
                    if (sum_cost pl.inv) > n && pl.color <> game.next_col then 
                      (sum_cost pl.inv, pl.color)
                    else (n, acc)) 
                  (0,Red) game.plist
            in
            if fst wrapped = 0 then None else Some (snd wrapped)
          in

          let most_player_inv = 
            if player_w_most <> None then
              let most_player_rec = 
                let wrapped =
                  List.fold_left 
                    (fun acc pl_rec -> 
                      if pl_rec.color = get_some player_w_most then
                        pl_rec::acc
                      else 
                        acc) 
                    [] game.plist
                in
                match wrapped with 
                | h::_ -> h
                | _ -> failwith "shouldn't happen."
              in
              most_player_rec.inv
            else
            (0, 0, 0, 0, 0)
          in
          print_endline (string_of_list string_of_color !players_w_res);
          incr num_trade;
          print_endline "I'm fucking trying to trade.";
          (*actual trading block*)
          if abun_or_unnec_res <> None then 
            match !players_w_res with 
            |h::t -> 
              players_w_res := t;
              (print_endline "NUMBER 1");
              print_endline "-------------------------------";
              print_endline "trying to get:";
              print_endline (string_of_cost (create_cost lack_res 1));
              print_endline "for";
              print_endline (string_of_cost (create_cost (get_some abun_or_unnec_res )1));
              print_endline "from";
              print_endline (string_of_color h);
              print_endline "-------------------------------";
              Action(DomesticTrade(h, create_cost (get_some abun_or_unnec_res) 1, create_cost lack_res 1))
            |[] -> 
              (print_endline "NUMBER 2");
              let ivegotnothing = my_inventory = (0,0,0,0,0) in
              if ivegotnothing && player_w_most <> None then
                let buff = memremove (fun x -> x = (get_some player_w_most)) !players_w_res in
                players_w_res := buff;
                Action(DomesticTrade(get_some player_w_most ,(0, 0, 0, 0, 0), most_player_inv))
              else 
                Action(MaritimeTrade(get_some abun_or_unnec_res, lack_res))
          else (*no abundunt resource*)
            match 0 with 
            | _ -> 
            print_endline "NUMBER 3";
            if player_w_most <> None then
              begin
              match 0 with 
              | _ -> 
              print_endline "-------------------------------";
              print_endline "trying to get:";
              print_endline (string_of_cost most_player_inv);
              print_endline (string_of_color (get_some player_w_most));
              print_endline "-------------------------------";
              let buff = memremove (fun x -> x = (get_some player_w_most)) !players_w_res in
              players_w_res := buff;
              Action(DomesticTrade(get_some player_w_most ,(0, 0, 0, 0, 0), most_player_inv))
            end
            else 
              failwith "????????"

        else (*num_trade >= 7*)
        match 0 with 
        |_ -> 
        num_trade:= 0;
        print_endline "not gonna trade!";
        print_endline "num_trade:";
        print_endline (string_of_int !num_trade);
        

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
          let bool = is_valid_cost cur_player_inv cCOST_TOWN in
(*           print_endline (string_of_bool bool); *)
          bool
        in      
        if enough_resources then
          match viable_point state with
          |None -> begin
            match viable_line state with
            |None -> 
              players_w_res:=[];
              Action(EndTurn)
            |Some l -> Action(BuyBuild(BuildRoad(c,l)))
          end
          |Some p -> Action(BuyBuild(BuildTown(p)))
        else
          match viable_line (b,p,t,n) with
          |None -> 
            players_w_res:=[];
            Action(EndTurn)
          |Some l -> Action(BuyBuild(BuildRoad(c,l)))
end

(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))


(* 
          let road = 
            let y_call_road cost = 
              let (b,w,o,g,l) = cost in
              if (lack [b;l]) <= 2 then
                let weird_fold (i,acc) e =
                  if i = 1 || i = 2 || i = 3 then 
                    (i+1,acc)
                  else 
                    if e = 0 then (i+1,i::acc)
                    else (i+1,acc)
                in
                let need_i = snd(List.fold_left weird_fold (0,[]) [b;w;o;g;l]) in
                let need_res = List.map index_res need_i in
                match need_res with
                |h::h1::_ -> (true,(h,h1))
                |h::_ -> (true,(h,h))
                |_ -> failwith "f"
              else (false,(Brick,Brick))
            in
            match y_call_road my_inventory with
            |(true, (r1, r2)) -> [r1;r2]
            |_ -> []
          in

          let dev = 
            let y_call_dev cost = 
              let (b,w,o,g,l) = cost in
               if (lack [w;o;g]) <= 2 then
                let weird_fold (i,acc) e =
                  if i = 0 || i = 4 then (i+1,acc)
                  else 
                    if e = 0 then (i+1,i::acc)
                    else (i+1,acc)
                in
                let need_i = snd(List.fold_left weird_fold (0,[]) [b;w;o;g;l]) in
                let need_res = List.map index_res need_i in
                match need_res with
                |h::h1::_ -> (true,(h,h1))
                |h::_ -> (true,(h,h))
                |_ -> failwith "f"
              else (false,(Brick,Brick))
            in
            match y_call_dev my_inventory with
            |(true, (r1, r2)) -> [r1;r2]
            |_ -> []
          in

          let city = 
            let y_call_city cost =
              let (b,w,o,g,l) = cost in
              if (o>=1 && g >=2) || (o>=3) || (o>=2 && g>=1) then begin
                if o >= 3 then
                  (true,(Grain,Grain))
                else if g >= 2 then
                  (true,(Ore,Ore))
                else (true,(Ore,Grain))
              end
              else (false,(Grain,Grain))
            in
            match y_call_city my_inventory with
            |(true, (r1, r2)) -> [r1;r2]
            |_ -> []
          in *)