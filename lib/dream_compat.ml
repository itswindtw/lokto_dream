(* Types *)
type request = Message.request
type response = Message.response
type handler = Message.handler
type middleware = Middleware.t
type route = Routing.route
type 'a message = 'a Message.t
type client = Message.client
type server = Message.server

(* Methods *)
type method_ = Method.t

let method_to_string = Method.to_string
let string_to_method = Method.of_string
let methods_equal = Method.equal
let normalize_method = Method.normalize

(* Status codes *)
type informational = Status.informational
type successful = Status.successful
type redirection = Status.redirection
type client_error = Status.client_error
type server_error = Status.server_error
type standard_status = Status.standard_status
type status = Status.t

let status_to_string = Status.to_string
let status_to_reason = Status.to_reason
let status_to_int = Status.to_int
let int_to_status = Status.of_int
let is_informational = Status.is_informational
let is_successful = Status.is_successful
let is_redirection = Status.is_redirection
let is_client_error = Status.is_client_error
let is_server_error = Status.is_server_error
let status_codes_equal = Status.equal
let normalize_status = Status.normalize

(* Requests *)
let client request = Requests.client
let tls request = Requests.tls
let method_ = Requests.method_
let target = Requests.target
let set_client = Requests.set_client
let set_method_ = Requests.set_method_
let query = Requests.query
let queries = Requests.queries
let all_queries = Requests.all_queries

(* Responses *)
let response = Responses.response
let respond = Responses.response
let html = Responses.html
let json = Responses.json

let redirect ?status ?code ?headers request location =
  Responses.redirect ?status ?code ?headers location

let empty = Responses.empty
let status = Responses.status
let set_status = Responses.set_status

(* Headers *)
let header = Headers.header
let headers = Headers.headers
let all_headers = Headers.all_headers
let has_header = Headers.has_header
let add_header = Headers.add_header
let drop_header = Headers.drop_header
let set_header = Headers.set_header

(* Cookies *)
let set_cookie = Cookies.set_cookie
let drop_cookie = Cookies.drop_cookie
let cookie = Cookies.cookie
let all_cookies = Cookies.all_cookies

(* Bodies *)
let body = Bodies.body
let set_body = Bodies.set_body

(* Streams *)
type stream = Stream.t

let body_stream = Requests.body_stream
let stream = Responses.stream
let read = Stream.read
let write = Stream.write
let flush = Stream.flush
let close = Stream.close

(* WebSockets *)
type websocket = Websockets.websocket

let websocket = Websockets.websocket

type text_or_binary = Websockets.text_or_binary
type end_of_message = Websockets.end_of_message

let send = Websockets.send
let receive = Websockets.receive
let receive_fragment = Websockets.receive_fragment
let close_websocket = Websockets.close_websocket

(* JSON *)
let origin_referrer_check = Middleware.origin_referrer_check

(* Forms *)
(* MAYBE *)

(* Templates *)
(* MAYBE: I'm not a fan of current approach *)

(* Middleware *)
let no_middleware = Middleware.empty
let pipeline = Middleware.pipeline

(* MAYBE *)
(* livereload *)
(* client_stream *)
(* server_stream *)
(* set_client_stream *)
(* set_server_stream *)

(* Routing *)
let router = Routing.router
let get = Routing.get
let post = Routing.post
let put = Routing.put
let delete = Routing.delete
let head = Routing.head
let connect = Routing.connect
let options = Routing.options
let trace = Routing.trace
let patch = Routing.patch
let any = Routing.any
let param = Routing.param
let scope = Routing.scope
let no_route = Routing.no_route

(* Static files *)
(* MAYBE *)

(* Sessions *)
let session_field = Sessions.session_field
let set_session_field = Sessions.set_session_field
let drop_session_field = Sessions.drop_session_field
let all_session_fields = Sessions.all_session_fields
let invalidate_session = Sessions.invalidate_session
let memory_sessions = Sessions.memory_sessions
let cookie_sessions = Sessions.cookie_sessions
let session_id = Sessions.session_id
let session_label = Sessions.session_label
let session_expires_at = Sessions.session_expires_at

(* MAYBE *)
(* sql_sessions *)

(* Flash messages *)
(* MAYBE *)

(* GraphQL *)
(* MAYBE *)

(* SQL *)
(* MAYBE *)

(* Logging *)
let logger = Middleware.logger

(* MAYBE: app should use its own logging source perhaps *)

(* Errors *)
(* MAYBE: Implement it when I need this *)

(* Servers *)
let serve = Server.serve
let run = Server.run
(* MAYBE: complete interface compatibility *)

(* Web formats *)
let html_escape = Web_formats.html_escape
let to_base64url = Web_formats.to_base64url
let from_base64url = Web_formats.from_base64url
let to_percent_encoded = Web_formats.to_percent_encoded
let from_percent_encoded = Web_formats.from_percent_encoded
let to_form_urlencoded = Web_formats.to_form_urlencoded
let from_form_urlencoded = Web_formats.from_form_urlencoded
let from_cookie = Web_formats.from_cookie

let to_set_cookie ?expires ?max_age ?domain ?path ?secure ?http_only ?same_site
    name value =
  let max_age = Option.map Float.to_int max_age in
  Web_formats.to_set_cookie ?expires ?max_age ?domain ?path ?secure ?http_only
    ?same_site name value

let split_target = Web_formats.split_target
let from_path = Web_formats.from_path
let to_path = Web_formats.to_path
let drop_trailing_slash = Web_formats.drop_trailing_slash
let text_html = Web_formats.text_html
let application_json = Web_formats.application_json

(* Cryptography *)
let set_secret = Cryptography.set_secret
let random = Cryptography.random
let encrypt = Cryptography.encrypt
let decrypt = Cryptography.decrypt

(* Variables *)
type 'a field = 'a Message.field

let new_field = Message.new_field
let field = Message.field
let set_field = Message.set_field

(* Testing *)
(* MAYBE *)
