module Dream = Lokto_dream.Dream_compat

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ fun request ->
  match Dream.session_field request "user" with
  | None ->
      Dream.invalidate_session request;
      Dream.set_session_field request "user" "alice";
      Dream.html "You weren't logged in; but now you are!"
  | Some username ->
      Printf.ksprintf Dream.html "Welcome back, %s!"
        (Dream.html_escape username)
