
val map: ('a -> 'b list -> 'b list)  -> 'a list -> 'b list
val map_from_to: (int -> 'a list -> 'a list)  -> int -> int -> 'a list
val map_from_downto: (int -> 'a list -> 'a list) -> int -> int  -> 'a list

val map_prepend: 
  ('a -> 'b list -> 'b list)  -> 'a list -> 'b list -> 'b list
val map_from_to_prepend: 
  (int -> 'a list -> 'a list)  -> int -> int -> 'a list -> 'a list
val map_from_downto_prepend: 
  (int -> 'a list -> 'a list) -> int -> int  -> 'a list -> 'a list

val rev : 'a list -> 'a list
val concat_map_prepend : 
  ('a -> 'b list -> 'b list) -> 'a list -> 'b list -> 'b list
val concat_map_from_to_prepend: 
  (int -> 'a list -> 'a list) -> int -> int -> 'a list -> 'a list
val concat_map_from_downto_prepend: 
  (int -> 'a list -> 'a list) -> int -> int -> 'a list -> 'a list
