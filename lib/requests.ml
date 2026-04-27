(* client *)
let client_field =
  Message.new_field ~name:"lokto_dream.client" ~show_value:Fun.id ()

let client request =
  match Message.field request client_field with
  | None -> ""
  | Some client -> client

let set_client request client = Message.set_field request client_field client

let sockaddr_to_client = function
  | Unix.ADDR_INET (addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port
  | Unix.ADDR_UNIX path -> path

(* tls *)
let tls_field =
  Message.new_field ~name:"lokto_dream.tls" ~show_value:string_of_bool ()

let tls request =
  match Message.field request tls_field with Some true -> true | _ -> false

let set_tls request tls = Message.set_field request tls_field tls

(* method *)
let method_ (request : Message.request) = request.envelope.method_

let set_method_ (request : Message.request) method_ =
  request.envelope.method_ <- (method_ :> Method.t)

(* target *)
let target (request : Message.request) = request.envelope.target

(* query *)
let all_queries request =
  let _, query = Web_formats.split_target (target request) in
  Web_formats.from_form_urlencoded query

let query request name = List.assoc_opt name (all_queries request)

let queries request name =
  all_queries request
  |> List.filter_map (fun (name', value) ->
      if name' = name then Some value else None)

(* request_body *)
let body_stream (request : Message.request) =
  match request.body with
  | String _ -> invalid_arg "body is already consumed"
  | Stream s -> s
