let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t

let rec last_two = function
  | []
  | [_] ->
      None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t

let rec at n l =
  match (n, l) with
  | n', _ when n' < 1 -> None
  | _, [] -> None
  | 1, h :: _ -> Some h
  | _, _ :: t -> at (pred n) t

let rev l =
  let rec f l' = function
    | [] -> l'
    | h :: t -> f (h :: l') t
  in
  f [] l

let is_palindrome l = l = rev l

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One x :: t -> x :: flatten t
  | Many xs :: t -> List.append (flatten xs) (flatten t)

let rec compress = function
  | [] -> []
  | h :: t ->
      let rec f = function
        | h' :: t' when h = h' -> f t'
        | l -> compress l
      in
      h :: f t

let rec pack l =
  match l with
  | [] -> []
  | h :: t ->
      let rec f l' =
        match l' with
        | h' :: t' when h = h' ->
            let g, t'' = f t' in
            (h' :: g, t'')
        | l -> ([], l)
      in
      let g, t' = f t in
      (h :: g) :: pack t'

type 'a rle =
  | One of 'a
  | Many of int * 'a

let rec encode = function
  | [] -> []
  | h :: t ->
      let rec f n = function
        | h' :: t' when h = h' -> f (succ n) t'
        | l -> (if n = 1 then One h else Many (n, h)) :: encode l
      in
      f 1 t

[@@@warning "-8"]

let encode' = function
  | [] -> []
  | h :: t ->
      List.fold_left
        (fun ((n, el) :: seq as acc) x ->
          if x = el then (succ n, el) :: seq else (1, x) :: acc)
        [(1, h)] t
      |> List.rev

[@@@warning "+8"]

let rec repeat x = function
  | 0 -> []
  | n -> x :: (repeat x @@ pred n)

let decode l =
  List.map
    (function
      | One x -> [x]
      | Many (n, x) -> repeat x n)
    l
  |> List.concat

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

let replicate l n = List.map (fun x -> repeat x n) l |> List.concat

let drop_every l n =
  let rec f i = function
    | [] -> []
    | h :: t -> if i == 1 then f n t else h :: f (pred i) t
  in
  f n l

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t

let rec split_at lst n =
  match (lst, n) with
  | [], _ -> (lst, [])
  | _, 0 -> ([], lst)
  | h :: t, n ->
      let l, l' = split_at t (pred n) in
      (h :: l, l')

let split l = split_at l @@ (length l / 2)

let rec merge l l' =
  match (l, l') with
  | h :: t, h' :: t' ->
    if h < h' then h :: merge t l' else h' :: merge l t'
  | h :: t, [] -> h :: merge t []
  | [], h' :: t' -> h' :: merge [] t'
  | [], [] -> []

(* Time complexity: n + 1.5n * log(n)   *)
let merge_sort lst =
  let rec f lst = function
    | 0
    | 1 ->
        lst
    | len ->
        let i = len / 2 in
        let l, l' = split_at lst i in
        merge (f l i) (f l' (len - i))
  in
  f lst (length lst)

let length' l =
  let rec len n = function
    | [] -> n
    | _ :: t -> len (succ n) t
  in
  len 0 l

let split_at' l n =
  let rec f acc i l' =
    match (i, l') with
    | 0, l' -> (rev acc, l')
    | _, [] -> (rev acc, [])
    | _, h :: t -> f (h :: acc) (pred i) t
  in
  f [] n l

let split' l = split_at' l @@ (length' l / 2)

let rec merge' acc l l' =
  match (l, l') with
  | h :: t, h' :: t' ->
      if h < h' then merge' (h :: acc) t l' else merge' (h' :: acc) l t'
  | h :: t, [] -> merge' (h :: acc) t []
  | [], h' :: t' -> merge' (h' :: acc) [] t'
  | [], [] -> rev acc

(* Time complexity: n + 3n * log(n)   *)
let merge_sort' lst =
  let rec f lst = function
    | 0
    | 1 ->
        lst
    | len ->
        let i = len / 2 in
        let l, l' = split_at' lst i in
        merge' [] (f l i) (f l' (len - i))
  in
  f lst (length' lst)
