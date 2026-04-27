type text_or_binary = [ `Text | `Binary ]
type end_of_message = [ `End_of_message | `Continues ]
type fragment = string * text_or_binary * end_of_message

type websocket = {
  wsd : H1.Websocket.Wsd.t;
  mutex : Miou.Mutex.t;
  condition : Miou.Condition.t;
  queue : fragment Queue.t;
  closed : bool ref;
}

let websocket_field : (websocket -> unit) Message.field =
  Message.new_field ~name:"lokto_dream.websocket"
    ~show_value:(fun _ -> "<thunk>")
    ()

let close_websocket websocket =
  H1.Websocket.Wsd.(if not (is_closed websocket.wsd) then close websocket.wsd)

let websocket ?headers ?(close = true) callback =
  let response = Responses.response ~status:`Switching_Protocols ?headers "" in

  Message.set_field response websocket_field (fun websocket ->
      Fun.protect
        ~finally:(fun () -> if close then close_websocket websocket)
        (fun () -> callback websocket));

  response

let send ?(text_or_binary = `Text) ?(end_of_message = `End_of_message) websocket
    data =
  (* Message.send ?text_or_binary ?end_of_message server_stream data *)
  let kind = text_or_binary in
  let is_fin =
    match end_of_message with `End_of_message -> true | `Continues -> false
  in
  let payload = Bytes.of_string data in
  let off = 0 in
  let len = Bytes.length payload in
  H1.Websocket.Wsd.send_bytes websocket.wsd ~kind ~is_fin (Bytes.of_string data)
    ~off ~len

let receive_fragment websocket =
  Miou.Mutex.protect websocket.mutex (fun () ->
      while Queue.is_empty websocket.queue && not !(websocket.closed) do
        Miou.Condition.wait websocket.condition websocket.mutex
      done;

      if Queue.is_empty websocket.queue then None
      else Some (Queue.pop websocket.queue))

let receive websocket =
  let buf = Buffer.create 4096 in
  let rec loop () =
    match receive_fragment websocket with
    | None -> None
    | Some (data, _kind, `End_of_message) ->
        Buffer.add_string buf data;
        Some (Buffer.contents buf)
    | Some (data, _kind, `Continues) ->
        Buffer.add_string buf data;
        loop ()
  in
  loop ()
