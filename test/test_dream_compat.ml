open Alcotest
module Dream = Lokto_dream.Dream_compat

let tests =
  [
    test_case "from_path" `Quick (fun () ->
        let examples =
          [
            ("", []);
            ("/", [ "" ]);
            ("abc", [ "abc" ]);
            ("/abc", [ "abc" ]);
            ("abc/", [ "abc"; "" ]);
            ("a%2Fb", [ "a/b" ]);
            ("a//b", [ "a"; "b" ]);
          ]
        in
        List.iter
          (fun (path, expected) ->
            let actual = Dream.from_path path in
            check (list string) path expected actual)
          examples);
    test_case "to_path" `Quick (fun () ->
        let examples =
          [
            (false, [], "/");
            (false, [ "a"; "b" ], "/a/b");
            (false, [ ""; "a"; ""; "b"; "" ], "/a/b/");
            (true, [], "");
            (true, [ "a"; "b" ], "a/b");
            (true, [ ""; "a"; ""; "b"; "" ], "a/b/");
          ]
        in
        List.iter
          (fun (relative, components, expected) ->
            let actual = Dream.to_path ~relative components in
            check string "" expected actual)
          examples);
    test_case "from_form_urlencoded" `Quick (fun () ->
        let examples =
          [
            ("", []);
            ( "username=john%20doe&message=Hello%20%26%20goodbye",
              [ ("username", "john doe"); ("message", "Hello & goodbye") ] );
          ]
        in

        List.iter
          (fun (encoded, expected) ->
            let actual = Dream.from_form_urlencoded encoded in
            check (list (pair string string)) encoded expected actual)
          examples);
  ]

let () = Alcotest.run "lokto_dream" [ ("Dream_compat", tests) ]
