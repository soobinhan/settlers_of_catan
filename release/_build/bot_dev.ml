open Definition
open Registry
open Constant
open Util

let name = "dev"


(* let get_t_and_r (i,acc) e = *)
(*   match e with *)
(*   |None -> (i+1,acc) *)
(*   |Some(c,_) -> if c = color then (i+1, (List.nth hex i)::acc) *)
(* in *)
(* let current = List.fold_left get_t_and_r (0,[]) ilist in *)
(* let dont_have' acc e = *)
(*   if List.mem_assoc e current then acc *)
(*   else e::acc *)
(* in *)
(* let dont_have = List.fold_left dont_have' []  *)
(*   [Hill;Pasture;Mountain;Field;Forest] in *)
(* let find_min (acc,roll) (t,r) = *)
(*   if roll_probs r < roll_probs roll then *)
(*     (t,r) *)
(*   else (acc,roll) *)
(* in *)
(* let min_terrain = fst(List.fold_left find_min (Desert,0) current) in *) 

module Bot = functor (S : Soul) -> struct
    
  let initialize () = ()

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
      if rest_g >= 3 then (true,Grain)
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
      if rest_l >= 3 then
        (true,Lumber)
      else (false,Lumber)
    else if l = 1 then
      let rest_b = sum_res_pl pl col Brick in
      if rest_b >= 3 then
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
    
  let handle_request (s : state) : move =
    let (_,p,t,n) = s in
    let (c, r) = n in
    let (color,(inv,cds),_) = List.find(fun (c',_,_) -> c' = c) p in
    let cards = 
      match cds with
      |Reveal l -> l
      |_ -> failwith "whattttt"
    in
    match r with
      | InitialRequest -> InitialMove(0, 0)
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) else
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
          else
            Action(EndTurn)
            
            
          
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
