open Definition
open Registry
open Constant
open Util

let name = "bot_rob"

module Bot = functor (S: Soul) -> struct

	let initialize () = ()

	let i = ref 0

	let handle_request ((board, plist, turn, next): state) : move = 
		let (c, req) = next in 
		match req with 
		| InitialRequest -> InitialMove(i:= !i + 1; !i, !i + 2)
		| RobberRequest -> RobberMove(10, Some Red)
      	| DiscardRequest-> RobberMove(10, Some Red)
      	| TradeRequest -> RobberMove(10, Some Red)
      	| ActionRequest -> failwith ""
end