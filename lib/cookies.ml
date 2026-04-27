let all_cookies message =
  Headers.headers message "Cookie"
  |> List.map Web_formats.from_cookie
  |> List.flatten

let infer_cookie_prefix prefix domain path secure =
  match (prefix, domain, path, secure) with
  | Some (Some `Host), _domain, _path, _secure -> "__Host-"
  | Some (Some `Secure), _domain, _path, _secure -> "__Secure-"
  | Some None, _domain, _path, _secure -> ""
  | None, None, "/", true -> "__Host-"
  | None, _domain, _path, true -> "__Secure-"
  | None, _domain, _path, _secure -> ""

let associated_data_prefix = "lokto_dream.cookie-"

let cookie ?prefix ?(decrypt = true) ?domain ?path ?secure request name =
  let path =
    match path with Some path -> path | None -> Routing.prefix request
  in
  let secure =
    match secure with Some secure -> secure | None -> Requests.tls request
  in

  let cookie_prefix = infer_cookie_prefix prefix domain path secure in
  let name = cookie_prefix ^ name in

  let ( let* ) = Option.bind in
  let* value = List.assoc_opt name (all_cookies request) in
  if decrypt then
    let* value = Web_formats.from_base64url value in
    Cryptography.decrypt request value
      ~associated_data:(associated_data_prefix ^ name)
  else Some value

let set_cookie ?prefix ?(encrypt = true) ?expires ?max_age ?domain ?path ?secure
    ?(http_only = true) ?(same_site = Some `Lax) response request name value =
  let path =
    match path with Some path -> path | None -> Routing.prefix request
  in
  let secure =
    match secure with Some secure -> secure | None -> Requests.tls request
  in

  let cookie_prefix = infer_cookie_prefix prefix domain path secure in

  let name = cookie_prefix ^ name in

  let value =
    if encrypt then
      Cryptography.encrypt request value
        ~associated_data:(associated_data_prefix ^ name)
      |> Web_formats.to_base64url
    else value
  in

  let set_cookie =
    Web_formats.to_set_cookie ?expires ?max_age ?domain ~path ~secure ~http_only
      ?same_site name value
  in

  Headers.add_header response "Set-Cookie" set_cookie

let drop_cookie ?prefix ?domain ?path ?secure ?http_only ?same_site response
    request name =
  set_cookie ?prefix ~encrypt:false ~expires:0. ?domain ?path ?secure ?http_only
    ?same_site response request name ""
