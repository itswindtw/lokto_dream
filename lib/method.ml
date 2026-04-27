type t =
  [ `GET
  | `POST
  | `PUT
  | `DELETE
  | `HEAD
  | `CONNECT
  | `OPTIONS
  | `TRACE
  | `PATCH
  | `Method of string ]

let to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `HEAD -> "HEAD"
  | `CONNECT -> "CONNECT"
  | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE"
  | `PATCH -> "PATCH"
  | `Method string -> string

let of_string s =
  match String.uppercase_ascii s with
  | "GET" -> `GET
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "DELETE" -> `DELETE
  | "HEAD" -> `HEAD
  | "CONNECT" -> `CONNECT
  | "OPTIONS" -> `OPTIONS
  | "TRACE" -> `TRACE
  | "PATCH" -> `PATCH
  | s -> `Method s

let normalize t =
  match (t :> t) with `Method string -> of_string string | t -> t

let equal a b = normalize a = normalize b
