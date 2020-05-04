module S = String
module L = List

type 'a container =
  { it : 'a;
    make_str : string -> string }

type int_record =
  { aaa : int;
    u : int;
    get_u : int -> int }

let make_int_record i =
  let aaa = 22 + i in
  let u = 33 in
  let get_u a = u + a in
  {aaa; u; get_u}

let container_of_int r =
  { it = r;
    make_str =
      (fun x ->
        S.concat ", " [x; string_of_int r.aaa; string_of_int (r.get_u 3)]) }

type lst_record =
  { bbb : string list;
    get_bbb : unit -> string list }

let make_lst_record l =
  let bbb = "22" :: l in
  let get_bbb () = bbb in
  {bbb; get_bbb}

let container_of_lst r =
  {it = r; make_str = (fun x -> S.concat ", " (x :: r.bbb))}

let concat_containers a b =
  let v = "foo" in
  (a, b, a.make_str v ^ " :: " ^ b.make_str v)

let res =
  let a = make_int_record 777 |> container_of_int in
  let b = make_lst_record ["777"] |> container_of_lst in
  match concat_containers a b with
  | (x, y, s) as r ->
      print_int (x.it.get_u 200);
      print_newline ();
      print_string (L.hd (y.it.get_bbb ()));
      print_newline ();
      print_string s;
      print_newline ();
      r

let res' =
  let a = make_lst_record ["777"] |> container_of_lst in
  let b = make_int_record 777 |> container_of_int in
  concat_containers a b

type 'a gg =
  { q : int;
    b : 'a container }

let b = {q = 2; b = make_int_record 4 |> container_of_int}
