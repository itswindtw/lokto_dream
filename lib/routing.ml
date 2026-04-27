type method_set = [ Method.t | `Any ]

type token = Literal of string | Param of string | Wildcard of string
and node = Handler of method_set * Message.handler | Scope of route
and route = (token list * node) list

let path_field : string list Message.field =
  Message.new_field ~name:"lokto_dream.path"
    ~show_value:(fun path -> String.concat "/" path)
    ()

let prefix_field =
  Message.new_field ~name:"lokto_dream.prefix"
    ~show_value:(fun prefix -> String.concat "/" (List.rev prefix))
    ()

let params_field =
  Message.new_field ~name:"lokto_dream.params"
    ~show_value:(fun params ->
      params
      |> List.map (fun (param, value) -> Printf.sprintf "%s=%s" param value)
      |> String.concat ", ")
    ()

let internal_prefix request =
  match Message.field request prefix_field with
  | Some prefix -> prefix
  | None -> []

let prefix request = Web_formats.to_path (List.rev (internal_prefix request))

let path request =
  match Message.field request path_field with
  | Some path -> path
  | None ->
      request |> Requests.target |> Web_formats.split_target |> fst
      |> Web_formats.from_path

let method_matches method_set method_ =
  match method_set with
  | `Any -> true
  | #Method.t as method' -> Method.equal method' method_

let rec apply middlewares routes =
  let rec compose handler = function
    | [] -> handler
    | middleware :: more -> middleware (compose handler more)
  in
  routes |> List.flatten
  |> List.map (fun (pattern, node) ->
      let node =
        match node with
        | Handler (method_, handler) ->
            Handler (method_, compose handler middlewares)
        | Scope route -> Scope (apply middlewares [ route ])
      in
      (pattern, node))

let rec strip_empty_trailing_token = function
  | [] -> []
  | [ Literal "" ] -> []
  | token :: tokens -> token :: strip_empty_trailing_token tokens

let rec validate route = function
  | [] -> ()
  | [ Wildcard "*" ] -> ()
  | Wildcard "*" :: _ -> failwith "Path wildcard must be last"
  | Wildcard _ :: _ -> failwith "Path wildcard must be just '**'"
  | Param "" :: _ -> Fmt.failwith "Empty path parameter name in '%s'" route
  | _ :: tokens -> validate route tokens

let parse raw =
  let tokens =
    String.split_on_char '/' raw
    |> List.filter_map (function
      | "" -> None
      | s when s.[0] = ':' ->
          Some (Param (String.sub s 1 (String.length s - 1)))
      | s when s.[0] = '*' -> (
          match String.sub s 1 (String.length s - 1) with
          | "" -> Some (Literal "*")
          | rest -> Some (Wildcard rest))
      | s -> Some (Literal s))
    |> function
    | [] -> [ Literal "" ]
    | tokens -> tokens
  in
  validate raw tokens;
  tokens

let under prefix routes =
  [ (strip_empty_trailing_token (parse prefix), Scope (List.flatten routes)) ]

let scope prefix middlewares routes = under prefix [ apply middlewares routes ]

let router routes =
  let routes = List.flatten routes in

  fun request ->
    let rec try_routes bindings prefix path = function
      | [] -> None
      | (pattern, node) :: rest -> (
          match try_route bindings prefix path pattern node with
          | None -> try_routes bindings prefix path rest
          | result -> result)
    and try_route bindings prefix path pattern node =
      match (pattern, path) with
      | [], _ -> try_node bindings prefix path node false
      | _, [] -> None
      | Literal s :: pattern, s' :: path when s = s' ->
          try_route bindings (s' :: prefix) path pattern node
      | Literal _ :: _, _ -> None
      | Param _ :: _, "" :: _ -> None
      | Param s :: pattern, s' :: path ->
          try_route ((s, s') :: bindings) (s' :: prefix) path pattern node
      | Wildcard _ :: _, _ -> try_node bindings prefix path node true
    and try_node bindings prefix path node is_wildcard =
      match node with
      | Handler (method_, handler)
        when method_matches method_ (Requests.method_ request) ->
          Message.set_field request params_field bindings;

          if is_wildcard then begin
            Message.set_field request prefix_field prefix;
            Message.set_field request path_field path;
            Some (handler request)
          end
          else if path = [] then Some (handler request)
          else None
      | Handler _ -> None
      | Scope routes -> try_routes bindings prefix path routes
    in
    let params =
      Option.value (Message.field request params_field) ~default:[]
    in
    let prefix = internal_prefix request in
    let path = path request in

    match try_routes params prefix path routes with
    | None -> Responses.empty `Not_Found
    | Some response -> response

let get pattern handler = [ (parse pattern, Handler (`GET, handler)) ]
let post pattern handler = [ (parse pattern, Handler (`POST, handler)) ]
let put pattern handler = [ (parse pattern, Handler (`PUT, handler)) ]
let delete pattern handler = [ (parse pattern, Handler (`DELETE, handler)) ]
let head pattern handler = [ (parse pattern, Handler (`HEAD, handler)) ]
let connect pattern handler = [ (parse pattern, Handler (`CONNECT, handler)) ]
let options pattern handler = [ (parse pattern, Handler (`OPTIONS, handler)) ]
let trace pattern handler = [ (parse pattern, Handler (`TRACE, handler)) ]
let patch pattern handler = [ (parse pattern, Handler (`PATCH, handler)) ]
let any pattern handler = [ (parse pattern, Handler (`Any, handler)) ]
let missing_param name = Fmt.failwith "Missing path parameter %S" name

let param request name =
  match
    Option.bind (Message.field request params_field) (List.assoc_opt name)
  with
  | None -> missing_param name
  | Some p -> p

let no_route = []
