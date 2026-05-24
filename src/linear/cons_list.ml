type 'a t = Nil | Cons of 'a * 'a t

let empty = Nil
let rec len = function Nil -> 0 | Cons (x, xs) -> 1 + len xs
let rec of_list = function [] -> Nil | x :: xs -> Cons (x, of_list xs)
let rec to_list = function Nil -> [] | Cons (x, xs) -> x :: to_list xs
let rec map f = function Nil -> Nil | Cons (x, xs) -> Cons (f x, map f xs)

let mapi f =
  let rec loop i = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f i x, loop (i + 1) xs)
  in
  loop 0

let map_full f xs =
  let rec loop i = function
    | Nil -> Nil
    | Cons (x, xs') -> Cons (f i x xs, loop (i + 1) xs')
  in
  loop 0 xs

let rec filter f = function
  | Nil -> Nil
  | Cons (x, xs) -> if f x then Cons (x, filter f xs) else filter f xs

let filteri f =
  let rec loop i = function
    | Nil -> Nil
    | Cons (x, xs) ->
        if f i x then Cons (x, loop (i + 1) xs) else loop (i + 1) xs
  in
  loop 0

let filter_full f xs =
  let rec loop i = function
    | Nil -> Nil
    | Cons (x, xs') ->
        if f i x xs then Cons (x, loop (i + 1) xs) else loop (i + 1) xs
  in
  loop 0 xs

let rec filter_map f = function
  | Nil -> Nil
  | Cons (x, xs) -> (
      match f x with
      | Some x' -> Cons (x', filter_map f xs)
      | None -> filter_map f xs)

let rec fold_left f acc = function
  | Nil -> acc
  | Cons (x, xs) -> fold_left f (f acc x) xs

let rec fold_right f acc = function
  | Nil -> acc
  | Cons (x, xs) -> f x @@ fold_right f acc xs

let reduce f = function Nil -> None | Cons (x, xs) -> Some (fold_left f x xs)
let cons x xs = Cons (x, xs)
let ( @ ) = cons

let rec take n = function
  | Nil -> Nil
  | Cons (x, xs) -> if n > 0 then Cons (x, take (n - 1) xs) else Nil

let rec take_while f = function
  | Nil -> Nil
  | Cons (x, xs) -> if f x then Cons (x, take_while f xs) else Nil

let rec drop n = function
  | Nil -> Nil
  | Cons (x, xs) as all -> if n <= 0 then all else drop (n - 1) xs

let rec drop_while f = function
  | Nil -> Nil
  | Cons (x, xs) as all -> if f x then drop_while f xs else all

let reverse xs = fold_left (fun acc x -> Cons (x, acc)) Nil xs
let append x xs = fold_right cons (Cons (x, Nil)) xs

let rec extend xs ys =
  match xs with Nil -> ys | Cons (x, xs) -> Cons (x, extend xs ys)

let flatten xs = fold_left (fun acc xs' -> extend acc xs') Nil xs

let rec find f = function
  | Nil -> None
  | Cons (x, xs) -> if f x then Some x else find f xs

let pure x = Cons (x, Nil)
let bind f xs = xs |> map f |> flatten

let%expect_test "Simple test" =
  print_string "sup";
  [%expect {| what's up |}]
