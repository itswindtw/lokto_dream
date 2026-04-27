let status (t : Message.response) = t.envelope.status
let set_status (t : Message.response) status = t.envelope.status <- status

let response ?status ?code ?headers body : Message.response =
  let response = Message.response ?status ?code ?headers (String body) in
  response

let html ?status ?code ?headers body =
  let response = response ?status ?code ?headers body in
  Headers.set_header response "Content-Type" Web_formats.text_html;
  response

let json ?status ?code ?headers body =
  let response = response ?status ?code ?headers body in
  Headers.set_header response "Content-Type" Web_formats.application_json;
  response

let redirect ?status ?code ?headers location =
  let status =
    match (status, code) with None, None -> Some `See_Other | _ -> status
  in
  let response = response ?status ?code ?headers "" in
  Headers.set_header response "Location" location;
  response

let empty ?headers status = response ?headers ~status ""

let stream_handler_field : (Stream.t -> unit) Message.field =
  Message.new_field ~name:"lokto_dream.stream"
    ~show_value:(fun _ -> "<thunk>")
    ()

let stream ?status ?code ?headers ?(close = true) callback =
  let stream = Stream.create () in
  let response = Message.response ?status ?code ?headers (Stream stream) in
  Message.set_field response stream_handler_field callback;
  response
