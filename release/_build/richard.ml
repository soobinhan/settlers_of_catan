open Definition
open Registry
open Constant
open Util
open Print

let name = "rick"

module Bot = functor (S: Soul) -> struct

  let num_trade = ref 0
  let players_w_res = ref []

  let initialize () = 
    num_trade:=0;
    players_w_res:=[];

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
        
  let sum_res_pl pl col res =
    let rest = list_memremove(fun (c,_,_) -> c = col) pl in
    let fold' acc (_,(i,_),_) =
      acc + (num_resource_in_inventory i res)
    in
    List.fold_left fold' 0 rest 

  let lack l = 
    List.fold_left(fun acc e -> if e = 0 then acc+1 else acc) 0 l

  let m_call_city s cost =
    let (_,pl,_,(col,_)) = s in
    let (b,w,o,g,l) = cost in
    if o = 3 then
      let rest_g = sum_res_pl pl col Grain in
      if rest_g >= 2 then (true,Grain)
      else (false,Grain)
    else if g = 2 then
      let rest_o = sum_res_pl pl col Ore in
      if rest_o >= 3 then (true,Ore)
      else (false,Ore)
    else (false, Ore) 

  let y_call_city s cost =
    let (b,w,o,g,l) = cost in
    if (o>=1 && g >=2) || (o>=3) || (o>=2 && g>=1) then begin
      if o >= 3 then
        (true,(Grain,Some Grain))
      else if g >= 2 then
        (true,(Ore,Some Ore))
      else (true,(Ore,Some Grain))
    end
    else (false,(Grain,Some Grain))
      
  let index_res = function
    |0 -> Brick
    |1 -> Wool
    |2 -> Ore
    |3 -> Grain
    |4 -> Lumber
    |_ -> failwith "no"
      
  let m_call_town s cost =
    let (_,pl,_,(col,_)) = s in
    let (b,w,o,g,l) = cost in
    if (lack [b;w;g;l]) = 1 then
      let weird_fold (i,acc) e =
        if i = 2 then (i+1,acc)
        else 
          if e = 0 then (i+1,i)
          else (i+1,acc)
      in
      let need_res = index_res 
        (snd(List.fold_left weird_fold (0,0) [b;w;o;g;l])) in
      if (sum_res_pl pl col need_res) >= 3 then
        (true, need_res)
      else (false, need_res)
    else (false, Brick)

  let y_call_town s cost = 
    let (b,w,o,g,l) = cost in
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
      |h::h1::_ -> (true,(h,Some h1))
      |h::_ -> (true,(h,Some h))
      |[] -> (false,(Brick, Some Brick))
    else (false,(Brick,Some Brick))

  let m_call_road s cost = 
    let (_,pl,_,(col,_)) = s in
    let (b,w,o,g,l) = cost in
    if b = 1 then
      let rest_l = sum_res_pl pl col Lumber in
      if rest_l >= 1 then
        (true,Lumber)
      else (false,Lumber)
    else if l = 1 then
      let rest_b = sum_res_pl pl col Brick in
      if rest_b >= 1 then
        (true,Brick)
      else (false,Brick)
    else (false,Brick)

  let y_call_road s cost = 
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
      |h::h1::_ -> (true,(h,Some h1))
      |h::_ -> (true,(h,Some h))
      |[] -> (false,(Brick, Some Brick))
    else (false,(Brick,Some Brick))

  let m_call_dev s cost = 
    let (_,pl,_,(col,_)) = s in
    let (b,w,o,g,l) = cost in
    if (lack [w;o;g]) = 1 then
      let weird_fold (i,acc) e =
        if i = 0 || i = 4 then (i+1,acc)
        else 
          if e = 0 then (i+1,i)
          else (i+1,acc)
      in
      let need_res = index_res 
        (snd(List.fold_left weird_fold (0,0) [b;w;o;g;l])) in
      if (sum_res_pl pl col need_res) >= 3 then
        (true,need_res)
      else (false,need_res)
    else (false,Brick)

  let y_call_dev s cost = 
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
      |h::h1::_ -> (true,(h,Some h1))
      |h::_ -> (true,(h,Some h))
      |_ -> (false,(Brick,Some Brick)) 
    else (false,(Brick,Some Brick))

  let roll_probs roll =
    match roll with
    |6 |8 -> 5
    |5 |9 -> 4
    |4 |10 -> 3
    |3 |11 -> 2
    |2 |12 -> 1
    |0 -> 0
    |_ -> failwith "this shouldn't happen yo"

  let call_knight s =
    let (((hex,_),(il,_),_,_,rob),pl,t,(color,_)) = s in
    let points = piece_corners rob in
    let fold acc e =
      match List.nth il e with
      |None -> acc
      |Some(c,_) -> c = color || acc 
    in
    let is_robbed = List.fold_left fold false points in
    if is_robbed then
      let count (bool,acc) e =
        match List.nth il e with
        |None -> (bool,acc)
        |Some (c,_) -> (c=color||bool,acc+1)
      in
      let fold' (i,acc) (_,r) =
        let (bool,num_s) = List.fold_left count (false,0) (piece_corners i) in
        if (not bool) && i <> rob then
          (i+1,(i,num_s,r)::acc)
        else (i+1,acc)
      in
      let hex_info = snd(List.fold_left fold' (0,[]) hex) in
      let find_max (i,num,r) (i',num',r') =
        if num' > num then (i',num',r')
        else if num = num' then begin
          if roll_probs r' > roll_probs r then
            (i',num',r')
          else (i,num,r)
        end
        else (i,num,r)
      in
      let (piece,_,_) = List.fold_left find_max (0,0,0) hex_info in
      let get_color acc e =
        match List.nth il e with
        |None -> acc
        |Some(c,_) -> c::acc
      in
      let possible_victims = List.fold_left get_color [] (piece_corners piece) in
      let find_victim (c,sum) (color,(inv,_),_) =
        if List.mem color possible_victims then
          (*can make this better if you want to but 
            right now keeping things simple*)
          if (sum_cost inv) > sum then
            (Some color,(sum_cost inv))
          else (c,sum)
        else (c,sum)
      in
      let color = fst(List.fold_left find_victim (None,0) pl) in
      (true,(piece,color))
    else
      (false,(0,None))

  let is_road_occupied rl (p1,p2) =
    let fold' acc (_,(p,p')) = acc || (p1 = p && p2 = p') || (p1 = p' && p2 = p) in
    List.fold_left fold' false rl

  let call_rb s =
    let ((_,(il,rl),_,_,rob),pl,t,(color,_)) = s in
    let viable_settle il p =
      (* let overlap = List.nth il p <> None in *)
      let adj_list = adjacent_points p in
      let adjacent = List.fold_left
        (fun acc x -> acc || List.nth il x <> None) false adj_list
      in
      not adjacent
    (* not (overlap || adjacent) *)
    in

    let create_random ()=
      let fold' (i,acc) e =
        match e with
        |None -> if (viable_settle il i) then (i+1,i::acc) else (i+1,acc) 
        |_ -> (i+1,acc)
      in
      let middle_pt = fst(pick_one(snd(List.fold_left fold' (0,[]) il))) in
      let (pick_other1,rest) = pick_one(adjacent_points middle_pt) in
      let pick_other2 = fst(pick_one rest) in
      ((color,(pick_other1,middle_pt)),Some(color,(middle_pt,pick_other2)))
    in
    
    create_random()

  let yop_possible s cost =
    (fst(y_call_town s cost))||(fst(y_call_city s cost))
    ||(fst(y_call_road s cost))||(fst(y_call_dev s cost))
      
  let m_possible s cost =
    (fst(m_call_town s cost))||(fst(m_call_city s cost))
    ||(fst(m_call_road s cost))||(fst(m_call_dev s cost))

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

  let viable_settle g p=
    let overlap = List.nth g.ilist p <> None in
    let adj_list = adjacent_points p in
    let adjacent = List.fold_left
      (fun acc x -> acc || List.nth g.ilist x <> None) false adj_list
    in
    not (overlap || adjacent)

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

  let decide_robber s = 
    let (((hex,_),(il,_),_,_,rob),pl,t,(color,_)) = s in
    let count (bool,acc) e =
      match List.nth il e with
      |None -> (bool,acc)
      |Some (c,_) -> (c=color||bool,acc+1)
    in
    let fold' (i,acc) (_,r) =
      let (bool,num_s) = List.fold_left count (false,0) (piece_corners i) in
      if (not bool) && i <> rob then
        (i+1,(i,num_s,r)::acc)
      else (i+1,acc)
    in
    let hex_info = snd(List.fold_left fold' (0,[]) hex) in
    let find_max (i,num,r) (i',num',r') =
      if num' > num then (i',num',r')
      else if num = num' then begin
        if roll_probs r' > roll_probs r then
          (i',num',r')
        else (i,num,r)
      end
      else (i,num,r)
    in
    let (piece,_,_) = List.fold_left find_max (0,0,0) hex_info in
    let get_color acc e =
      match List.nth il e with
      |None -> acc
      |Some(c,_) -> c::acc
    in
    let possible_victims = List.fold_left get_color [] (piece_corners piece) in
    let find_victim (c,sum) (color,(inv,_),_) =
      if List.mem color possible_victims then
        (*can make this better if you want to but 
          right now keeping things simple*)
        if (sum_cost inv) > sum then
          (Some color,(sum_cost inv))
        else (c,sum)
      else (c,sum)
    in
    let color = fst(List.fold_left find_victim (None,0) pl) in
    piece,color


  let viable_settle il p=
    let overlap = List.nth il p <> None in
    let adj_list = adjacent_points p in
    let adjacent = List.fold_left 
      (fun acc x -> acc || List.nth il x <> None) false adj_list
    in
    not (overlap || adjacent)

  let vlist_town (b,p,t,(color,_)) =
    let ((hex,_),(il,rl),_,_,_) = b in
    let find acc (c,l) =
      if c = color then l::acc else acc
    in
    let my_rl = List.fold_left find [] rl in
    let viable acc (p1,p2) = 
      let p1_bool = (not (List.mem p1 acc)) && (viable_settle il p1) in
      let p2_bool = (not (List.mem p2 acc)) && (viable_settle il p2) in
      match p1_bool,p2_bool with
      |true,true -> p1::p2::acc
      |true,false -> p1::acc
      |false,true -> p2::acc
      |false,false -> acc
    in
    List.fold_left viable [] my_rl

 

  let vlist_road (b,p,t,(color,_)) =
    let ((hex,_),(il,rl),_,_,_) = b in
    let special_linked rl p =
      let road' acc (p1,p2) = acc || (p = p1 || p = p2) in
      List.fold_left road' false rl
    in
    let find acc (c,l) =
      if c = color then l::acc else acc
    in
    let my_rl = List.fold_left find [] rl in
    print_endline ("rl: "^ (string_of_int (List.length my_rl))); 
    let viable acc (p1,p2) = 
      let fold' acc' p =
        if (not (List.mem p acc')) && (not (special_linked my_rl p))
          && (List.nth il p = None) then
          p::acc' 
        else acc'
      in
      let p1 = List.fold_left fold' acc (adjacent_points p1) in
      List.fold_left fold' p1 (adjacent_points p2)
    in
    List.fold_left viable [] my_rl
        

  let get_other rl col p = 
    let road_linked rl col p =
      let road' acc (c,(p1,p2)) = acc || (c=col && (p = p1 || p = p2)) in
      List.fold_left road' false rl
    in
    let other = 
    List.fold_left
      (fun acc e -> if road_linked rl col e then e else acc)
      0 (adjacent_points p)
    in
    print_endline ("other: "^(string_of_int other));
    other


  let build_settle (b,p,t,(color,_)) viable_pts =
    print_endline ("viable: "^ (string_of_list string_of_int viable_pts)); 
    let ((hex,_),(il,rl),_,_,_) = b in
    let get_t_and_r (i,acc) e =
      match e with
      |None -> (i+1,acc)
      |Some(c,_) -> 
        if c = color then
          let fold4 acc' e = 
            (List.nth hex e)::acc' 
          in
          (i+1,List.fold_left fold4 acc (adjacent_pieces i))
        else (i+1,acc)
    in
    let current = snd(List.fold_left get_t_and_r (0,[]) il) in
    let dont_have' acc e =
      if List.mem_assoc e current then acc
      else e::acc
    in
    let dont_have = List.fold_left dont_have' []
      [Hill;Pasture;Mountain;Field;Forest] in
    let find_mins (acc,roll) (t,r) =
      if roll_probs r < roll_probs roll then
        (t,r)
      else (acc,roll)
    in
    let needed = (fst(List.fold_left find_mins (Desert,0) current))::dont_have in
    let fold1 acc p =
      let fold2 acc' i =
        let (t,r) = List.nth hex i in
        if List.mem t needed then
          (p,t,r)::acc'
        else acc'
      in
      List.fold_left fold2 acc (adjacent_pieces p) 
    in
    let good_pts = List.fold_left fold1 [] viable_pts in 
    let find_max3 (i',t',r') (i,t,r) =
      if (roll_probs r) >= r' then
        (i,t,(roll_probs r))
      else (i',t',r')
    in
    let (i,_,_) = List.fold_left find_max3 (0,Desert,0) good_pts in
    i


  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)
  (***********************************************************)

  let turn_color = ref Red 

  let handle_request (s : state) : move =
    let (b,p,t,n) = s in
    (if !turn_color <> t.active then begin
      turn_color := t.active;
      num_trade := 0;
      players_w_res := [];
      print_endline "I'm here"
    end
    else ()
    );
    let ((hex,_),(il,rl),_,_,_) = b in
    let (c, r) = n in
    let state = (b, p, t, n) in
    let game = game_of_state state in
    let me = List.find (fun x -> x.color = c) game.plist in
    let my_inventory = me.inv in
    let (color,(inv,cds),_) = List.find(fun (c',_,_) -> c' = c) p in
    let cards = 
      match cds with
      |Reveal l -> l
      |_ -> failwith "whattttt"
    in
    match r with
    | InitialRequest -> InitialMove(optimal_initial state)
    | RobberRequest -> RobberMove(decide_robber state)
    | DiscardRequest-> DiscardMove(0,0,0,0,0)

    | TradeRequest -> 
      let me = List.find (fun x -> x.color = c) game.plist in
      let my_inventory = me.inv in
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
          let std = 4 in
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
                   if (fold' false inv_lst) && (p.color <> game.next_col) then p.color::acc else acc
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
                match 0 with _ ->
                print_endline "-------------------------------";
                print_endline "trying to get:";
                print_endline (string_of_cost most_player_inv);
                print_endline (string_of_color (get_some player_w_most));
                print_endline "-------------------------------";
                Action(DomesticTrade(get_some player_w_most ,(0, 0, 0, 0, 0), most_player_inv))
              else 
                match 0 with _ ->
                print_endline "-------------------------------";
                print_endline "trying to get from maritime:";
                print_endline (string_of_resource lack_res);
                print_endline "and give away:";
                print_endline (string_of_resource (get_some abun_or_unnec_res));
                print_endline "-------------------------------";
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
                match 0 with _ ->
                print_endline "Ending turn....";
                initialize ();
                Action(EndTurn)

        else (*num_trade >= 7*)

          match me.cards with 
          | Reveal(h::_) ->  (*I have some dev cards*)
            print_endline "I have cards";

            let s = state in
            if (List.mem YearOfPlenty cards) && (yop_possible s inv) then begin
              if fst(y_call_city s inv) then
                let (r,r') = snd(y_call_city s inv) in
                Action(PlayCard(PlayYearOfPlenty((r,r'))))
              else if fst(y_call_town s inv) then
                let (r,r') = snd(y_call_town s inv) in
                Action(PlayCard(PlayYearOfPlenty((r,r'))))
              else if fst(y_call_dev s inv) then
                let (r,r') = snd(y_call_dev s inv) in
                Action(PlayCard(PlayYearOfPlenty((r,r'))))
              else 
                let (r,r') = snd(y_call_road s inv) in
                Action(PlayCard(PlayYearOfPlenty((r,r'))))
            end
            else if (List.mem Monopoly cards) && (m_possible s inv) then begin
              if fst(m_call_city s inv) then
                Action(PlayCard(PlayMonopoly(snd(m_call_city s inv))))
              else if fst(m_call_town s inv) then
                Action(PlayCard(PlayMonopoly(snd(m_call_town s inv))))
              else if fst(m_call_dev s inv) then
                Action(PlayCard(PlayMonopoly(snd(m_call_dev s inv))))
              else Action(PlayCard(PlayMonopoly(snd(m_call_road s inv))))
            end
            else if (List.mem Knight cards) && (fst (call_knight s)) then
              Action(PlayCard(PlayKnight(snd(call_knight s))))
            else if List.mem RoadBuilding cards then
              let (r,r') = call_rb s in
              Action(PlayCard(PlayRoadBuilding((r,r'))))
            else begin
              match 0 with _ ->
              print_endline "ending turn.";
              initialize ();
              Action(EndTurn)
            end


          | _ -> (*i don't have any dev cards*)
          print_endline "I don't have any cards";


          let enough_resources cost = 
            let is_valid_cost current_inv cost = 
              (map_cost2 (fun e e' -> e>= e') current_inv cost) = (true,true,true,true,true)
            in
            is_valid_cost inv cost
          in 
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
          if List.length settlement_list >= 5 || enough_resources cCOST_CITY then
            let best_spot = 
              let score point = 
                let adj_list = adjacent_pieces point in
                List.fold_left (fun acc piece -> acc + roll_probs (snd (List.nth (game.hex_list) piece))) 0 adj_list
              in
              let tuplelist =
                List.fold_left (fun acc point -> (score point, point)::acc) [] settlement_list
              in
              snd(
                List.fold_left 
                  (fun (score, point) tuple -> 
                    if (fst tuple) > point then 
                      tuple 
                    else 
                      (score, point))
                  (0, 0) 
                  tuplelist)
            in
            print_endline "building a city";
            Action(BuyBuild(BuildCity(best_spot)))
          else 
            match 0 with _ ->
            print_endline "building road / town";
            if enough_resources cCOST_TOWN then
              let vlist = vlist_town s in
              match vlist with
              |[] -> 
                let i = build_settle s (vlist_road s) in
                let other = get_other rl c i in
                Action(BuyBuild(BuildRoad(c,(i,other))))
              |_ -> 
                let i = build_settle s (vlist_town s) in
                Action(BuyBuild(BuildTown(i)))
            else if enough_resources cCOST_ROAD then
              let i = build_settle s (vlist_road s) in
              let other = get_other rl c i in
              Action(BuyBuild(BuildRoad(c,(i,other))))
            else if enough_resources cCOST_CARD then
              Action(BuyBuild(BuildCard))
            else
              match 0 with _ ->
              print_endline "Ending turn.";
              initialize ();
              Action(EndTurn)
           
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
