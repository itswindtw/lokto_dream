let option_value_or_thunk ~default = function Some v -> v | None -> default ()

let on_sigint () =
  let mutex = Miou.Mutex.create () in
  let condition = Miou.Condition.create () in
  let flag = ref false in
  let stop _sigint =
    Miou.Mutex.protect mutex (fun () ->
        flag := true;
        Miou.Condition.broadcast condition)
  in
  let wait () =
    Miou.Mutex.protect mutex (fun () ->
        while !flag = false do
          Miou.Condition.wait condition mutex
        done)
  in
  ignore (Miou.sys_signal Sys.sigint (Sys.Signal_handle stop));
  wait

let serve ?(interface = "localhost") ?(port = 3000) ?stop handler =
  let handler = Middleware.default_error_handler handler in
  let stop = option_value_or_thunk stop ~default:on_sigint in

  let sockaddr =
    match Unix.getaddrinfo interface (Int.to_string port) [] with
    | [] -> failwith "getaddrinfo"
    | addr_info :: _ -> addr_info.ai_addr
  in

  let socket =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> failwith "UNIX sockets are not supported"
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
  in

  Miou_unix.bind_and_listen socket sockaddr;

  let domains = Miou.Domain.available () in

  let call = if domains >= 2 then Miou.call ?pin:None else Miou.async in

  let rec clean_up orphans =
    match Miou.care orphans with
    | None | Some None -> ()
    | Some (Some prm) -> (
        match Miou.await prm with
        | Ok () -> clean_up orphans
        | Error exn ->
            let backtrace = Printexc.get_backtrace () in
            Logging.err (fun m ->
                m "Unexpected exception: %s" (Printexc.to_string exn));
            if backtrace <> "" then Logging.err (fun m -> m "%s" backtrace);
            clean_up orphans)
  in

  let rec terminate orphans =
    match Miou.care orphans with
    | None -> Miou.yield ()
    | Some None ->
        Miou.yield ();
        terminate orphans
    | Some (Some prm) -> (
        match Miou.await prm with
        | Ok _ -> terminate orphans
        | Error exn ->
            let backtrace = Printexc.get_backtrace () in
            Logging.err (fun m ->
                m "Unexpected exception: %s" (Printexc.to_string exn));
            if backtrace <> "" then Logging.err (fun m -> m "%s" backtrace);
            terminate orphans)
  in

  let accept_or_stop ~stop socket =
    let accept = Miou.async (fun () -> `Accept (Miou_unix.accept socket)) in
    let or_stop =
      Miou.async (fun () ->
          stop ();
          `Stop)
    in
    match Miou.await_first [ accept; or_stop ] with
    | Ok value -> value
    | Error exn -> raise exn
  in

  let handle_ws ~stop ~user_handler socket =
    let wsd_cell = ref None in
    let wsd_mutex = Miou.Mutex.create () in
    let wsd_condition = Miou.Condition.create () in

    let websocket_mutex = Miou.Mutex.create () in
    let websocket_condition = Miou.Condition.create () in
    let websocket_queue = Queue.create () in
    let websocket_closed = ref false in

    let websocket_handler wsd : H1.Websocket.input_handlers =
      Miou.Mutex.protect wsd_mutex (fun () ->
          wsd_cell := Some wsd;
          Miou.Condition.signal wsd_condition);

      let last_text_or_binary = ref `Text in
      let frame_handler ~opcode ~is_fin frame ~off ~len =
        match opcode with
        | `Text | `Binary ->
            let text_or_binary =
              match opcode with
              | `Text -> `Text
              | `Binary -> `Binary
              | _ -> assert false
            in
            last_text_or_binary := text_or_binary;
            let end_of_message =
              if is_fin then `End_of_message else `Continues
            in
            let msg = Bstr.sub_string frame ~off ~len in
            let fragment = (msg, text_or_binary, end_of_message) in
            Miou.Mutex.protect websocket_mutex (fun () ->
                Queue.push fragment websocket_queue;
                Miou.Condition.signal websocket_condition)
        | `Continuation ->
            let end_of_message =
              if is_fin then `End_of_message else `Continues
            in
            let msg = Bstr.sub_string frame ~off ~len in
            let fragment = (msg, !last_text_or_binary, end_of_message) in
            Miou.Mutex.protect websocket_mutex (fun () ->
                Queue.push fragment websocket_queue;
                Miou.Condition.signal websocket_condition)
        | `Connection_close ->
            Miou.Mutex.protect websocket_mutex (fun () ->
                websocket_closed := true;
                Miou.Condition.signal websocket_condition)
        | `Ping -> H1.Websocket.Wsd.send_pong wsd
        | `Pong | `Other _ -> ()
      in
      let eof () =
        Miou.Mutex.protect websocket_mutex (fun () ->
            websocket_closed := true;
            Miou.Condition.signal websocket_condition)
      in
      { frame_handler; eof }
    in

    let connection = H1.Websocket.Server_connection.create ~websocket_handler in
    let bstr = Bstr.create H1.Config.default.read_buffer_size in
    let read_buffer = Bytes.create H1.Config.default.read_buffer_size in

    let write_yield_mutex = Miou.Mutex.create () in
    let write_yield_condition = Miou.Condition.create () in

    let rec read () =
      match H1.Websocket.Server_connection.next_read_operation connection with
      | `Close -> ()
      | `Read ->
          let n =
            Miou_unix.read socket ~len:(Bytes.length read_buffer) read_buffer
          in
          if n = 0 then
            ignore
              (H1.Websocket.Server_connection.read_eof connection Bstr.empty
                 ~off:0 ~len:0)
          else (
            Bstr.blit_from_bytes read_buffer ~src_off:0 bstr ~dst_off:0 ~len:n;
            ignore
              (H1.Websocket.Server_connection.read connection bstr ~off:0 ~len:n));
          read ()
    in

    let rec write () =
      match H1.Websocket.Server_connection.next_write_operation connection with
      | `Close _ -> ()
      | `Write iovecs ->
          let result =
            List.fold_left
              (fun acc { Faraday.buffer; len; off } ->
                match acc with
                | `Closed -> `Closed
                | `Ok n -> (
                    match
                      Miou_unix.write socket (Bstr.sub_string buffer ~off ~len)
                    with
                    | () -> `Ok (n + len)
                    | exception
                        Unix.Unix_error (Unix.EBADF, "check_descriptor", _) ->
                        `Closed
                    | exception exn -> raise exn))
              (`Ok 0) iovecs
          in

          H1.Websocket.Server_connection.report_write_result connection result;
          write ()
      | `Yield ->
          H1.Websocket.Server_connection.yield_writer connection (fun () ->
              Miou.Mutex.protect write_yield_mutex (fun () ->
                  Miou.Condition.signal write_yield_condition));

          Miou.Mutex.protect write_yield_mutex (fun () ->
              Miou.Condition.wait write_yield_condition write_yield_mutex);

          write ()
    in

    let prm_user =
      Miou.async (fun () ->
          let wsd =
            Miou.Mutex.protect wsd_mutex (fun () ->
                while !wsd_cell = None do
                  Miou.Condition.wait wsd_condition wsd_mutex
                done;
                Option.get !wsd_cell)
          in

          let websocket : Websockets.websocket =
            {
              wsd;
              mutex = websocket_mutex;
              condition = websocket_condition;
              queue = websocket_queue;
              closed = websocket_closed;
            }
          in

          user_handler websocket)
    in

    let prm_read_write =
      Miou.async (fun () ->
          let prm_read = Miou.async read in
          let prm_write = Miou.async write in

          Miou.await_all [ prm_read; prm_write ]
          |> List.iter (function Ok () -> () | Error exn -> raise exn))
    in

    let prm_stop = Miou.async (fun () -> stop ()) in

    match Miou.await_first [ prm_user; prm_read_write; prm_stop ] with
    | Ok () -> ()
    | Error exn -> raise exn
  in

  let handle_h1 ~stop ~sockaddr socket =
    let orphans = Miou.orphans () in

    Fun.protect ~finally:(fun () -> Miou_unix.close socket) @@ fun () ->
    let config = H1.Config.default in
    let websocket_user_handler = ref None in

    let request_handler reqd =
      try
        let h1_request = H1.Reqd.request reqd in
        let method_ =
          h1_request.meth |> H1.Method.to_string |> Method.of_string
        in
        let target = h1_request.target in
        let headers = H1.Headers.to_list h1_request.headers in
        let body_reader = H1.Reqd.request_body reqd in

        let stream =
          let mutex = Miou.Mutex.create () in
          let conditon = Miou.Condition.create () in
          let state = ref `Pending in

          let pop () =
            Miou.Mutex.protect mutex (fun () -> state := `Pending);

            H1.Body.Reader.schedule_read body_reader
              ~on_eof:(fun () ->
                Miou.Mutex.protect mutex (fun () ->
                    state := `Eof;
                    Miou.Condition.signal conditon))
              ~on_read:(fun bstr ~off ~len ->
                let s = Bstr.sub_string bstr ~off ~len in
                Miou.Mutex.protect mutex (fun () ->
                    state := `Data s;
                    Miou.Condition.signal conditon));

            Miou.Mutex.protect mutex (fun () ->
                while !state = `Pending do
                  Miou.Condition.wait conditon mutex
                done;
                match !state with
                | `Data s -> Stream.Data s
                | `Eof -> Stream.Eof
                | `Pending -> assert false)
          in
          Stream.create ~pop ()
        in

        let request =
          Message.request ~method_ ~target ~headers (Stream stream)
        in
        Requests.(set_client request (sockaddr_to_client sockaddr));

        (* *)
        let response = handler request in
        (* *)

        match Message.field response Websockets.websocket_field with
        | None -> (
            let headers = H1.Headers.of_list (Headers.all_headers response) in
            let status =
              Responses.status response |> Status.to_int |> H1.Status.of_code
            in

            match response.body with
            | String string ->
                let headers =
                  H1.Headers.add_unless_exists headers "Content-Length"
                    (Int.to_string (String.length string))
                in
                let h1_response = H1.Response.create ~headers status in
                H1.Reqd.respond_with_string reqd h1_response
                  (Bodies.body response)
            | Stream _stream ->
                let headers =
                  H1.Headers.add_unless_exists headers "Transfer-Encoding"
                    "chunked"
                in

                let h1_response = H1.Response.create ~headers status in
                let body_writer =
                  H1.Reqd.respond_with_streaming reqd h1_response
                in

                let stream =
                  let mutex = Miou.Mutex.create () in
                  let condition = Miou.Condition.create () in
                  let queue = Queue.create () in

                  let pop () =
                    Miou.Mutex.protect mutex (fun () ->
                        while Queue.is_empty queue do
                          Miou.Condition.wait condition mutex
                        done;
                        Queue.pop queue)
                  in
                  let push chunk =
                    Miou.Mutex.protect mutex (fun () ->
                        Queue.push chunk queue;
                        Miou.Condition.signal condition)
                  in
                  Stream.create ~pop ~push ()
                in

                let stream_handler =
                  Message.field response Responses.stream_handler_field
                  |> Option.get
                in

                ignore
                  (Miou.async ~orphans (fun () ->
                       let prm = Miou.async (fun () -> stream_handler stream) in

                       let rec loop () =
                         match stream.pop () with
                         | Data s ->
                             H1.Body.Writer.write_string body_writer s;
                             loop ()
                         | Flush ->
                             H1.Body.Writer.flush body_writer (fun () ->
                                 loop ())
                         | Eof -> H1.Body.Writer.close body_writer
                       in
                       loop ();

                       Miou.await_exn prm)))
        | Some handler ->
            websocket_user_handler := Some handler;

            let nonce =
              H1.Websocket.Handshake.get_nonce h1_request |> Option.get
            in
            let headers =
              H1.Websocket.Handshake.server_headers ~sha1:Cryptography.sha1
                ~nonce
            in
            let headers =
              H1.Headers.add_list headers (Headers.all_headers response)
            in

            H1.Reqd.respond_with_upgrade reqd headers
      with exn ->
        let backtrace = Printexc.get_backtrace () in
        Logging.err (fun m ->
            m "Unexpected exception: %s" (Printexc.to_string exn));
        if backtrace <> "" then Logging.err (fun m -> m "%s" backtrace);

        H1.Reqd.report_exn reqd exn
    in

    let connection = H1.Server_connection.create ~config request_handler in

    let bstr = Bstr.create config.read_buffer_size in
    let read_buffer = Bytes.create config.read_buffer_size in

    let read_yield_mutex = Miou.Mutex.create () in
    let read_yield_condition = Miou.Condition.create () in

    let write_yield_mutex = Miou.Mutex.create () in
    let write_yield_condition = Miou.Condition.create () in

    let rec read () =
      match H1.Server_connection.next_read_operation connection with
      | `Close ->
          terminate orphans;
          `Closed
      | `Read ->
          let n =
            Miou_unix.read socket ~len:config.read_buffer_size read_buffer
          in
          if n = 0 then
            ignore
              (H1.Server_connection.read_eof connection Bstr.empty ~off:0 ~len:0)
          else (
            Bstr.blit_from_bytes read_buffer ~src_off:0 bstr ~dst_off:0 ~len:n;
            ignore (H1.Server_connection.read connection bstr ~off:0 ~len:n));
          read ()
      | `Upgrade -> `Upgraded
      | `Yield ->
          H1.Server_connection.yield_reader connection (fun () ->
              Miou.Mutex.protect read_yield_mutex (fun () ->
                  Miou.Condition.signal read_yield_condition));
          Miou.Mutex.protect read_yield_mutex (fun () ->
              Miou.Condition.wait read_yield_condition read_yield_mutex);

          read ()
    in

    let rec write () =
      match H1.Server_connection.next_write_operation connection with
      | `Close _ -> `Closed
      | `Upgrade -> `Upgraded
      | `Write iovecs ->
          let result =
            List.fold_left
              (fun acc { Faraday.buffer; len; off } ->
                match acc with
                | `Closed -> `Closed
                | `Ok n -> (
                    match
                      Miou_unix.write socket (Bstr.sub_string buffer ~off ~len)
                    with
                    | () -> `Ok (n + len)
                    | exception
                        Unix.Unix_error (Unix.EBADF, "check_descriptor", _) ->
                        `Closed
                    | exception exn -> raise exn))
              (`Ok 0) iovecs
          in

          H1.Server_connection.report_write_result connection result;
          write ()
      | `Yield ->
          H1.Server_connection.yield_writer connection (fun () ->
              Miou.Mutex.protect write_yield_mutex (fun () ->
                  Miou.Condition.signal write_yield_condition));

          Miou.Mutex.protect write_yield_mutex (fun () ->
              Miou.Condition.wait write_yield_condition write_yield_mutex);

          write ()
    in

    let prm_read_write =
      Miou.async (fun () ->
          let prm_read = Miou.async read in
          let prm_write = Miou.async write in

          match (Miou.await prm_read, Miou.await prm_write) with
          | Ok `Upgraded, Ok `Upgraded -> `Upgraded
          | Ok `Closed, Ok `Closed -> `Closed
          | Ok _, Ok _ -> assert false
          | _, Error exn | Error exn, _ -> raise exn)
    in

    let prm_stop =
      Miou.async (fun () ->
          stop ();
          `Stop)
    in

    match Miou.await_first [ prm_read_write; prm_stop ] with
    | Ok `Upgraded ->
        let user_handler = Option.get !websocket_user_handler in
        handle_ws ~stop ~user_handler socket
    | Ok `Closed | Ok `Stop -> ()
    | Error exn -> raise exn
  in

  let rec loop orphans =
    clean_up orphans;
    match accept_or_stop ~stop socket with
    | `Stop ->
        terminate orphans;
        Miou_unix.close socket
    | `Accept (client, sockaddr) ->
        ignore (call ~orphans (fun () -> handle_h1 ~stop ~sockaddr client));
        loop orphans
  in

  loop (Miou.orphans ())

let run ?interface ?port ?stop handler =
  Miou_unix.run (fun () ->
      Sys.(set_signal sigpipe Signal_ignore);

      let mutex = Miou.Mutex.create () in
      Logs.set_reporter_mutex
        ~lock:(fun () -> Miou.Mutex.lock mutex)
        ~unlock:(fun () -> Miou.Mutex.unlock mutex);
      Logs.set_reporter (Logs_fmt.reporter ());
      Logs.set_level (Some Logs.Info);

      let stop = option_value_or_thunk stop ~default:on_sigint in
      let server () = serve ?interface ?port ~stop handler in

      let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in

      Fun.protect
        ~finally:(fun () -> Mirage_crypto_rng_miou_unix.kill rng)
        (fun () ->
          let prm = Miou.async server in
          Miou.await_exn prm))
