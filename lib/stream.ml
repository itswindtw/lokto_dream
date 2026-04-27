type chunk = Data of string | Flush | Eof
type t = { pop : unit -> chunk; push : chunk -> unit }

let not_supported _ = failwith "not_supported"
let create ?(pop = not_supported) ?(push = not_supported) () = { pop; push }

let rec read t =
  match t.pop () with Data s -> Some s | Flush -> read t | Eof -> None

let read_until_close stream =
  let buf = Buffer.create 1024 in
  let rec loop () =
    match read stream with
    | None -> Buffer.contents buf
    | Some s ->
        Buffer.add_string buf s;
        loop ()
  in
  loop ()

let write t s = t.push (Data s)
let flush t = t.push Flush
let close t = t.push Eof
