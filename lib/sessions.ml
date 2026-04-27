type session = {
  id : string;
  label : string;
  mutable expires_at : float;
  mutable payload : (string * string) list;
}

type handle = {
  put : string -> string -> unit;
  drop : string -> unit;
  invalidate : unit -> unit;
  mutable dirty : bool;
}

type 'a backend = {
  load : Message.request -> 'a;
  send : 'a -> Message.request -> Message.response -> Message.response;
}

let session_cookie = "lokto_dream.session"
let two_weeks_float_s = Float.of_int (60 * 60 * 24 * 7 * 2)

let field =
  Message.new_field ~name:session_cookie
    ~show_value:(fun (_, session) ->
      !session.payload
      |> List.map (fun (key, value) -> Printf.sprintf "%S: %S" key value)
      |> String.concat ", "
      |> Printf.sprintf "%s [%s]" !session.label)
    ()

let middleware backend next_handler request =
  let state = backend.load request in
  Message.set_field request field state;
  let response = next_handler request in
  backend.send state request response

let get_state request =
  match Message.field request field with
  | None -> failwith "Missing session middleware"
  | Some session -> session

let session request = !(snd (get_state request))
let handle request = fst (get_state request)

(* Public APIs *)

let all_session_fields request = (session request).payload
let session_field request key = List.assoc_opt key (all_session_fields request)
let set_session_field request key value = (handle request).put key value
let drop_session_field request key = (handle request).drop key
let invalidate_session request = (handle request).invalidate ()
let session_id request = (session request).id
let session_label request = (session request).label
let session_expires_at request = (session request).expires_at

(* Shared functions among backends *)

let now () = Ptime.to_float_s (Ptime_clock.now ())

let put_payload session key value =
  session.payload <- (key, value) :: List.remove_assoc key session.payload

let drop_payload session key =
  session.payload <- List.remove_assoc key session.payload

let generate_id () = Cryptography.random 18 |> Web_formats.to_base64url
let generate_label () = Cryptography.random 9 |> Web_formats.to_base64url

let new_session expires_at =
  { id = generate_id (); label = generate_label (); expires_at; payload = [] }

let refresh_if_old ~lifetime ~now session =
  if session.expires_at -. now > lifetime /. 2. then (false, session)
  else (
    session.expires_at <- now +. lifetime;
    (true, session))

(* Backends *)

module Memory = struct
  let table = lazy (Hashtbl.create 256)

  let make_handle ~table ~lifetime ~session ~dirty =
    let rec handle =
      {
        put = (fun key value -> put_payload !session key value);
        drop = (fun key -> drop_payload !session key);
        invalidate =
          (fun () ->
            Hashtbl.remove table !session.id;
            let expires_at = now () +. lifetime in
            session := new_session expires_at;
            Hashtbl.replace table !session.id !session;
            handle.dirty <- true);
        dirty;
      }
    in
    handle

  let load ~lifetime request =
    let table = Lazy.force table in
    let now = now () in

    let existing =
      let ( let* ) = Option.bind in
      let* session_id = Cookies.cookie ~decrypt:false request session_cookie in
      let* session = Hashtbl.find_opt table session_id in

      if session.expires_at > now then Some session
      else (
        Hashtbl.remove table session.id;
        None)
    in

    let dirty, session =
      match existing with
      | Some session -> refresh_if_old ~lifetime ~now session
      | None ->
          let s = new_session (now +. lifetime) in
          Hashtbl.replace table s.id s;
          (true, s)
    in
    let session = ref session in
    (make_handle ~table ~lifetime ~session ~dirty, session)

  let send (handle, session) request response =
    (if handle.dirty then
       let max_age = !session.expires_at -. now () |> Float.to_int in
       Cookies.set_cookie response request session_cookie !session.id
         ~encrypt:false ~max_age);
    response

  let backend ~lifetime = { load = load ~lifetime; send }
end

module Cookie = struct
  let encode session =
    `Assoc
      [
        ("id", `String session.id);
        ("label", `String session.label);
        ("expires_at", `Float session.expires_at);
        ( "payload",
          `Assoc (List.map (fun (k, v) -> (k, `String v)) session.payload) );
      ]
    |> Yojson.Basic.to_string

  let decode raw =
    let ( let* ) = Option.bind in
    match Yojson.Basic.from_string raw with
    | `Assoc
        [
          ("id", `String id);
          ("label", `String label);
          ("expires_at", expires_at);
          ("payload", `Assoc payload);
        ] ->
        let* expires_at =
          match expires_at with
          | `Float n -> Some n
          | `Int n -> Some (Float.of_int n)
          | _ -> None
        in
        let* payload =
          List.fold_left
            (fun acc (k, v) ->
              let* acc = acc in
              match v with `String s -> Some ((k, s) :: acc) | _ -> None)
            (Some []) payload
        in
        Some { id; label; expires_at; payload = List.rev payload }
    | _ -> None
    | exception _ -> None

  let make_handle ~lifetime ~session ~dirty =
    let rec handle =
      {
        put =
          (fun key value ->
            put_payload !session key value;
            handle.dirty <- true);
        drop =
          (fun key ->
            drop_payload !session key;
            handle.dirty <- true);
        invalidate =
          (fun () ->
            let expires_at = now () +. lifetime in
            session := new_session expires_at;
            handle.dirty <- true);
        dirty;
      }
    in
    handle

  let load ~lifetime request =
    let now = now () in

    let existing =
      let ( let* ) = Option.bind in
      let* cookie = Cookies.cookie request session_cookie in
      let* session = decode cookie in
      if session.expires_at > now then Some session else None
    in

    let dirty, session =
      match existing with
      | Some session -> refresh_if_old ~lifetime ~now session
      | None -> (true, new_session (now +. lifetime))
    in
    let session = ref session in
    (make_handle ~lifetime ~session ~dirty, session)

  let send (handle, session) request response =
    if handle.dirty then begin
      let max_age = !session.expires_at -. now () |> Float.to_int in
      Cookies.set_cookie response request session_cookie (encode !session)
        ~max_age
    end;
    response

  let backend ~lifetime = { load = load ~lifetime; send }
end

let memory_sessions ?(lifetime = two_weeks_float_s) =
  let backend = Memory.backend ~lifetime in
  middleware backend

let cookie_sessions ?(lifetime = two_weeks_float_s) =
  middleware (Cookie.backend ~lifetime)
