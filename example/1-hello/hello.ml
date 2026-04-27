module Dream = Lokto_dream.Dream_compat

let () = Dream.run (fun _ -> Dream.html "Good morning, world!")
