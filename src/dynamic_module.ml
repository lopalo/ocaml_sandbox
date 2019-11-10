module S = String
module L = List

module type Container = sig
  val make_str : string -> string
end

module type Int_arg = sig
  val v : int
end

module type Int_container = sig
  val get_u : int -> int

  include Container
end

module Make_int_container (I : Int_arg) : Int_container = struct
  let aaa = 22 + I.v

  let u = 33

  let get_u a = u + a

  let make_str x = S.concat ", " [x; string_of_int aaa; string_of_int (get_u 3)]
end

module type Lst_arg = sig
  val v : string list
end

module type Lst_container = sig
  val bbb : string list

  include Container
end

module Make_lst_container (L : Lst_arg) = struct
  let bbb = "22" :: L.v

  let make_str x = S.concat ", " (x :: bbb)
end

let concat_containers a b =
  let v = "foo" in
  let module A = (val a : Container) in
  let module B = (val b : Container) in
  (a, b, A.make_str v ^ " :: " ^ B.make_str v)

let res =
  let a =
    (module Make_int_container (struct
      let v = 777
    end) : Int_container)
  in
  let b =
    (module Make_lst_container (struct
      let v = ["777"]
    end) : Lst_container)
  in
  match concat_containers a b with
  | ((module X), (module Y), s) as r ->
      print_int (X.get_u 200);
      print_newline ();
      print_string (L.hd Y.bbb);
      print_newline ();
      print_string s;
      print_newline ();
      r

let res =
  let a =
    (module Make_lst_container (struct
      let v = ["777"]
    end) : Lst_container)
  in
  let b =
    (module Make_int_container (struct
      let v = 777
    end) : Int_container)
  in
  concat_containers a b
