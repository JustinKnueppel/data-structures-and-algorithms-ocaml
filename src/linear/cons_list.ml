type 'a t = Nil | Cons of 'a * 'a t

let rec to_string to_str xs =
  let rec build_list = function
    | Nil -> []
    | Cons (x, xs') -> to_str x :: build_list xs'
  in
  let components = List.concat [ [ "[" ]; build_list xs; [ "]" ] ] in
  String.concat " " components

let%expect_test "empty representation" =
  Nil |> to_string string_of_int |> print_string;
  [%expect {| [ ] |}]

let%expect_test "non-empty representation" =
  Cons (1, Cons (2, Nil)) |> to_string string_of_int |> print_string;
  [%expect {| [ 1 2 ] |}]

let rec of_list = function [] -> Nil | x :: xs -> Cons (x, of_list xs)

let%expect_test "empty list" =
  [] |> of_list |> to_string string_of_int |> print_string;
  [%expect {| [ ] |}]

let%expect_test "non-empty list" =
  [ 1; 2; 3 ] |> of_list |> to_string string_of_int |> print_string;
  [%expect {| [ 1 2 3 ] |}]

let rec to_list = function Nil -> [] | Cons (x, xs) -> x :: to_list xs

let%expect_test "convert_empty" =
  let lst_to_str = function
    | [] -> "[ ]"
    | lst ->
        let items = String.concat ", " lst in
        String.concat " " [ "["; items; "]" ]
  in
  let lists = [ Nil; Cons ("1", Cons ("2", Nil)) ] in
  List.iter (fun lst -> lst |> to_list |> lst_to_str |> print_endline) lists;
  [%expect {| 
  [ ] 
  [ 1, 2 ]
    |}]

let empty = Nil
let rec len = function Nil -> 0 | Cons (x, xs) -> 1 + len xs

let%expect_test "empty has length 0" =
  print_int (len empty);
  [%expect {| 0 |}]

let%expect_test "non-empty has correct length" =
  [ 1; 2; 3 ] |> of_list |> len |> print_int;
  [%expect {| 3 |}]

let rec map f = function Nil -> Nil | Cons (x, xs) -> Cons (f x, map f xs)

let%expect_test "map tests" =
  let f x = x * x in
  let table = [ ("empty", []); ("non-empty", [ 1; 2; 3 ]) ] in
  List.iter
    (fun (name, lst) ->
      Printf.printf "Case: %s -> %s\n" name
        (lst |> of_list |> map f |> to_string string_of_int))
    table;

  [%expect {|
    Case: empty -> [ ]
    Case: non-empty -> [ 1 4 9 ]
    |}]

let mapi f =
  let rec loop i = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f i x, loop (i + 1) xs)
  in
  loop 0

let%expect_test "mapi tests" =
  let f i x = (i * 100) + x in
  let table = [ ("empty", []); ("non-empty", [ 1; 2; 3 ]) ] in
  List.iter
    (fun (name, lst) ->
      Printf.printf "Case: %s -> %s\n" name
        (lst |> of_list |> mapi f |> to_string string_of_int))
    table;

  [%expect
    {|
    Case: empty -> [ ]
    Case: non-empty -> [ 1 102 203 ]
    |}]

let map_full f xs =
  let rec loop i = function
    | Nil -> Nil
    | Cons (x, xs') -> Cons (f i x xs, loop (i + 1) xs')
  in
  loop 0 xs

let%expect_test "map_full tests" =
  let f i x xs =
    Format.sprintf "%s at %d: %d\n" (to_string string_of_int xs) i x
  in
  let table = [ ("empty", []); ("non-empty", [ 1; 2; 3 ]) ] in
  List.iter
    (fun (name, lst) ->
      Printf.printf "Case: %s -> %s\n" name
        (lst |> of_list |> map_full f |> to_string (fun x -> x)))
    table;

  [%expect
    {|
    Case: empty -> [ ]
    Case: non-empty -> [ [ 1 2 3 ] at 0: 1
     [ 1 2 3 ] at 1: 2
     [ 1 2 3 ] at 2: 3
     ]
    |}]

let rec filter f = function
  | Nil -> Nil
  | Cons (x, xs) -> if f x then Cons (x, filter f xs) else filter f xs

let%expect_test "filter tests" =
  let f x = x mod 2 == 0 in
  let table = [ ("empty", []); ("non-empty", [ 1; 2; 3 ]) ] in
  List.iter
    (fun (name, lst) ->
      Printf.printf "Case: %s -> %s\n" name
        (lst |> of_list |> filter f |> to_string string_of_int))
    table;

  [%expect {|
    Case: empty -> [ ]
    Case: non-empty -> [ 2 ]
    |}]

let filteri f =
  let rec loop i = function
    | Nil -> Nil
    | Cons (x, xs) ->
        if f i x then Cons (x, loop (i + 1) xs) else loop (i + 1) xs
  in
  loop 0

let%expect_test "filteri tests" =
  let f i x = i == 0 || x mod 2 == 0 in
  let table = [ ("empty", []); ("non-empty", [ 1; 2; 3 ]) ] in
  List.iter
    (fun (name, lst) ->
      Printf.printf "Case: %s -> %s\n" name
        (lst |> of_list |> filteri f |> to_string string_of_int))
    table;

  [%expect {|
    Case: empty -> [ ]
    Case: non-empty -> [ 1 2 ]
    |}]

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
