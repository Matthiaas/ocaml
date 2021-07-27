
let map f y = 
  let rec loop f y res = 
    match y with 
    | [] -> res
    | l::ls -> loop f ls (f l res) 
  in
  List.rev (loop f y [])

let map_from_to f from to_ =
  let rec loop f from to_ res =
    if to_ < from 
    then res 
    else loop f from (to_ - 1) (f to_ res)
  in
  loop f from to_ []
;;

let map_from_downto f from to_ =
  let rec loop f from to_ res =
    if to_ > from 
    then res 
    else loop f from (to_ + 1) (f to_ res)
  in
  loop f from to_ [] 
;;

let map_prepend f l res =
  List.fold_left (fun acc el -> (f el acc )) res l

let map_from_to_prepend  f from to_ res=
  let rec loop f from to_ res =
    if to_ < from 
    then res 
    else loop f (from + 1) to_ (f from res)
  in
  loop f from to_ res
;;

let map_from_downto_prepend  f from to_ res =
  let rec loop f from to_ res =
    if to_ > from 
    then res 
    else loop f (from - 1) to_ (f from res)
  in
  loop f from to_ res
;;

(* Definitions for recursive usage.*)
(*
  TODO: Change to not use List.concat
*)

let rev = List.rev;;

let concat_map_prepend f l res= 
  List.fold_left (fun acc el -> f el acc ) res l
;;

let concat_map_from_to_prepend f from to_ res = 
  let rec loop f from to_ res =
    if to_ < from 
    then res 
    else loop f (from + 1) to_ ((f from res))
  in
  loop f from to_ res
;;

let concat_map_from_downto_prepend f from to_ res= 
  let rec loop f from to_ res =
    if to_ > from 
    then res 
    else loop f (from - 1) to_ ((f from res))
  in
  loop f from to_ res
;;