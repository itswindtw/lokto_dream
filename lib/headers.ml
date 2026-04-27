let all_headers (message : 'a Message.t) = message.headers

let header message name =
  let name = String.lowercase_ascii name in
  all_headers message
  |> List.find_map (fun (name', value) ->
      if String.lowercase_ascii name' = name then Some value else None)

let headers message name =
  let name = String.lowercase_ascii name in
  all_headers message
  |> List.filter_map (fun (name', value) ->
      if String.lowercase_ascii name' = name then Some value else None)

let has_header message name = Option.is_some (header message name)

let add_header (message : 'a Message.t) name value =
  message.headers <- (name, value) :: message.headers

let drop_header (message : 'a Message.t) name =
  let name = String.lowercase_ascii name in
  let headers =
    List.filter
      (fun (name', _) -> String.lowercase_ascii name' <> name)
      (all_headers message)
  in
  message.headers <- headers

let set_header t key value =
  drop_header t key;
  add_header t key value
