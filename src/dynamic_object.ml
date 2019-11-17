module S = String
module L = List

class int_container i =
  object (self)
    val aaa = 22 + i

    val u = 33

    method get_u a = u + a

    method make_str x =
      S.concat ", " [x; string_of_int aaa; string_of_int (self#get_u 3)]
  end

class lst_container l =
  object
    val bbb = "22" :: l

    method get_bbb = bbb

    method make_str x = S.concat ", " (x :: bbb)
  end

let concat_containers a b =
  let v = "foo" in
  (a, b, a#make_str v ^ " :: " ^ b#make_str v)

let res =
  let a = new int_container 777 in
  let b = new lst_container ["777"] in
  match concat_containers a b with
  | (x, y, s) as r ->
      print_int (x#get_u 200);
      print_newline ();
      print_string (L.hd y#get_bbb);
      print_newline ();
      print_string s;
      print_newline ();
      r

let res' =
  let a = new lst_container ["777"] in
  let b = new int_container 777 in
  concat_containers a b

type 'a gg =
  { q : int;
    b : 'a }
  constraint 'a = < make_str : string -> string ; .. >

let b = {q = 2; b = new int_container 4}

