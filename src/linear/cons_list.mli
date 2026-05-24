type 'a t = Nil | Cons of 'a * 'a t

val empty : 'a t
val len : 'a t -> int
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val map_full : (int -> 'a -> 'a t -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
val filter_full : (int -> 'a -> 'a t -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a option
val cons : 'a -> 'a t -> 'a t
val take : int -> 'a t -> 'a t
val take_while : ('a -> bool) -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val drop_while : ('a -> bool) -> 'a t -> 'a t
val reverse : 'a t -> 'a t
val append : 'a -> 'a t -> 'a t
val extend : 'a t -> 'a t -> 'a t
val flatten : 'a t t -> 'a t
val find : ('a -> bool) -> 'a t -> 'a option
val pure : 'a -> 'a t
val bind : ('a -> 'b t) -> 'a t -> 'b t
