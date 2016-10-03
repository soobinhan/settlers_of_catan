(** All bot modules should be opened here. *)

open Babybot
open Bot_init
open Bot_trade
open Bot_init
open Bot_rob
open Bot_buildroad
open Bot_dev
open Soobin
open Richard
open Richard2

(*
Initial move: stone and wheat
and make cities as quick as possible
and trade stone and wheat to other people
because once you make a settlement, super easy to turn'em into cities
go for largest army too
if resources to kill, get dev cards
if high threat player, don't trade
if low threat player, help them.
Using priority queue to rank the shit you want the bot to do. *)
