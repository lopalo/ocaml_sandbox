val check : unit -> int list

val message : string

type ('a, 'b) t =
  | Foo
  | Bar of 'a
  | Baz of 'a * 'b
  | Qux of
      { x : string;
        y : 'b }

val z : (int, string) t

val q : int

val f : ('a, 'b) t -> 'c -> string
