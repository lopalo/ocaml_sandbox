type 'a cell = {items : 'a list}

type 'a unpacker = {apply : 'b. 'b cell -> 'a} [@@unboxed]

type packed_cell = {unpack : 'a. 'a unpacker -> 'a} [@@unboxed]

let pack cell = {unpack = (fun unpacker -> unpacker.apply cell)}

let length packed_cell =
  packed_cell.unpack {apply = (fun {items} -> List.length items)}

let is_empty packed_cell =
  packed_cell.unpack
    { apply =
        (fun {items} ->
          match items with
          | [] -> true
          | _ -> false) }

let a = pack {items = ["value"]}

let b = pack {items = [1; 2; 3; 4; 5]}

let c = pack {items = ([] : unit list)}

let res =
  ([a; b; c], length a, length b, length c, is_empty a, is_empty b, is_empty c)
