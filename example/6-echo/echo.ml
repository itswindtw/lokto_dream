module Dream = Lokto_dream.Dream_compat

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.post "/echo" (fun request ->
             let body = Dream.body request in
             Dream.respond
               ~headers:[ ("Content-Type", "application/octet-stream") ]
               body);
       ]
