open Definition
open Registry
open Constant
open Util
open Print

(** Give your bot a 2-20 character name. *)
let name = "soobin"


module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  let roll_probs roll =
    match roll with
    |6 |8 -> 5
    |5 |9 -> 4
    |4 |10 -> 3
    |3 |11 -> 2
    |2 |12 -> 1
    |0 -> 0
    |_ -> failwith "this shouldn't happen yo"

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

      
      
    
  (* Invalid moves are overridden in game *)
  let handle_request (s : state) : move =
    let (b,p,t,(c,r)) = s in
    let ((hex,_),(il,rl),_,_,_) = b in
    let (_,(inv,_),_) = List.find(fun (col,_,_) -> col = c) p in
    match r with
      | InitialRequest -> InitialMove(0, 0)
      | RobberRequest -> RobberMove(decide_robber s)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) 
        else 
          let enough_resources = 
            let is_valid_cost current_inv cost = 
              (map_cost2 (fun e e' -> e>= e') current_inv cost) = (true,true,true,true,true)
            in
            is_valid_cost inv cCOST_TOWN
          in      
          if enough_resources then
            let vlist = vlist_town s in
            match vlist with
            |[] -> 
              let i = build_settle s (vlist_road s) in
              let other = get_other rl c i in
              Action(BuyBuild(BuildRoad(c,(i,other))))
            |_ -> 
              let i = build_settle s (vlist_town s) in
              Action(BuyBuild(BuildTown(i)))
          else
              let i = build_settle s (vlist_road s) in
              let other = get_other rl c i in
              Action(BuyBuild(BuildRoad(c,(i,other))))
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
