let set_body (t : 'a Message.t) body = t.body <- String body

let body (t : 'a Message.t) =
  match t.body with
  | String string -> string
  | Stream stream ->
      let string = Stream.read_until_close stream in
      set_body t string;
      string
