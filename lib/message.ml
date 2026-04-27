type 'a field_key_info = {
  name : string option;
  show_value : ('a -> string) option;
}

module Fields = Hmap.Make (struct
  type 'a t = 'a field_key_info
end)

type 'a field = 'a Fields.key
type body = String of string | Stream of Stream.t
type client = { mutable method_ : Method.t; target : string }
type server = { mutable status : Status.t }

type 'a t = {
  envelope : 'a;
  mutable headers : (string * string) list;
  mutable body : body;
  mutable fields : Fields.t;
}

type request = client t
type response = server t
type handler = request -> response

let request ?(method_ = `GET) ?(target = "/") ?(headers = []) body =
  let envelope = { method_; target } in
  { envelope; headers; body; fields = Fields.empty }

let response ?status ?code ?(headers = []) body =
  let status =
    match (status, code) with
    | None, None -> `OK
    | Some status, _ -> status
    | None, Some code -> Status.of_int code
  in
  let envelope = { status } in

  { envelope; headers; body; fields = Fields.empty }

(* fields *)

let new_field ?name ?show_value () = Fields.Key.create { name; show_value }
let field message key = Fields.find key message.fields

let set_field message key value =
  message.fields <- Fields.add key value message.fields
