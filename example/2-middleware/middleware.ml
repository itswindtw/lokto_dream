module Dream = Lokto_dream.Dream_compat

let () = Dream.run @@ Dream.logger @@ fun _ -> Dream.html "Good morning, world!"
