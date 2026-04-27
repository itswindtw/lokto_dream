let () = at_exit (fun () -> Gc.print_stat stdout)

let () =
  Dream.run
  @@ Dream.router
       [ Dream.get "/plaintext" (fun _ -> Dream.respond "Hello, World!") ]
