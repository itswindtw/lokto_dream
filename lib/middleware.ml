type t = Message.handler -> Message.handler

let empty next_handler request = next_handler request

let rec pipeline ts handler =
  match ts with [] -> handler | t :: ts -> t (pipeline ts handler)

let default_error_handler next_handler request =
  match next_handler request with
  | response -> response
  | exception exn ->
      let backtrace = Printexc.get_backtrace () in
      Logging.warn (fun m ->
          m "Unhandled exception: %s" (Printexc.to_string exn));
      if backtrace <> "" then Logging.warn (fun m -> m "%s" backtrace);

      Responses.empty `Internal_Server_Error

let logger next_handler request =
  let counter = Mtime_clock.counter () in

  (* Turn on backtrace recording. *)
  if not (Printexc.backtrace_status ()) then Printexc.record_backtrace true;

  Logging.info (fun m ->
      m "%s %s"
        (Method.to_string (Requests.method_ request))
        (Requests.target request));

  let response = next_handler request in

  let status = Responses.status response in

  let report level =
    let elapsed = Mtime_clock.count counter in
    Logging.msg level (fun m ->
        m "%i in %a" (Status.to_int status) Mtime.Span.pp elapsed)
  in

  let level =
    if Status.is_server_error status then Logs.Error
    else if Status.is_client_error status then Logs.Warning
    else Logs.Info
  in
  report level;

  response

let origin_referrer_check next_handler request =
  match Requests.method_ request with
  | `GET | `HEAD -> next_handler request
  | _ -> (
      let origin =
        match Headers.header request "Origin" with
        | None | Some "null" -> Headers.header request "Referer"
        | Some _ as origin -> origin
      in

      match origin with
      | None ->
          Logging.warn (fun m ->
              m "Origin and Referer headers are both missing");
          Responses.empty `Bad_Request
      | Some raw_origin -> (
          match Headers.header request "Host" with
          | None ->
              Logging.warn (fun log -> log "Host header is missing");
              Responses.empty `Bad_Request
          | Some raw_host ->
              let origin = Uri.of_string raw_origin in
              let host = Uri.of_string raw_host in

              let is_scheme_matched =
                match Uri.scheme origin with
                | Some "http" -> not (Requests.tls request)
                | Some "https" -> Requests.tls request
                | _ -> false
              in

              let is_host_matched = Uri.host origin = Uri.host host in
              let is_port_matched = Uri.port origin = Uri.port host in

              if is_scheme_matched && is_host_matched && is_port_matched then
                next_handler request
              else begin
                Logging.warn (fun m ->
                    m "Origin-Host mismatch: '%s' vs. '%s'" raw_origin raw_host);

                Responses.empty `Bad_Request
              end))
