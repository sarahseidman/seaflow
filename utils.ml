open Ast
open Sast
open Printf
open Random

(* Collection of utility functions *)
let rec list_last list = match list with 
  | [] -> failwith "List is empty"
  | [x] -> x
  | first_el::rest_of_list -> list_last rest_of_list