let check () =
  let q = 44 in
  if q == 33 then [] else [1; 2; 3]

let message = "Foo message"

type ('a, 'b) t =
  | Foo
  | Bar of 'a
  | Baz of 'a * 'b
  | Qux of {x : string; y : 'b}

let z = Baz (2, "dd")

let q = 1

let f a _ =
  match a with
  | Foo ->
      "foo!!"
  | Bar _ ->
      "bar"
  | Baz (_, _) ->
      "baz!!"
  | Qux {x; _} ->
      "qux " ^ x
