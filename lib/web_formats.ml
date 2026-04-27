let html_escape s =
  let buffer = Buffer.create (String.length s * 2) in
  s
  |> String.iter (function
    | '&' -> Buffer.add_string buffer "&amp;"
    | '<' -> Buffer.add_string buffer "&lt;"
    | '>' -> Buffer.add_string buffer "&gt;"
    | '"' -> Buffer.add_string buffer "&quot;"
    | '\'' -> Buffer.add_string buffer "&#x27;"
    | c -> Buffer.add_char buffer c);
  Buffer.contents buffer

let to_base64url =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet

let from_base64url s =
  Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s
  |> Result.to_option

let iri_generic =
  let safe = String.init 128 (fun i -> Char.chr (i + 128)) in
  `Custom (`Generic, safe, "")

let to_percent_encoded ?(international = true) string =
  let component = if international then iri_generic else `Generic in
  Uri.pct_encode ~component string

let from_percent_encoded = Uri.pct_decode

let to_form_urlencoded pairs =
  pairs
  |> List.map (fun (name, value) -> (name, [ value ]))
  |> Uri.encoded_of_query

let from_form_urlencoded encoded =
  match encoded with
  | "" -> []
  | encoded ->
      Uri.query_of_encoded encoded
      |> List.map (fun (name, values) -> (name, String.concat "," values))

let from_cookie s =
  let strip_dquote s =
    let n = String.length s in
    if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2)
    else s
  in

  let parse_pair raw =
    match String.index_opt raw '=' with
    | None -> None
    | Some i ->
        let name = String.trim (String.sub raw 0 i) in
        let value =
          String.trim (String.sub raw (i + 1) (String.length raw - (i + 1)))
          |> strip_dquote
        in
        if name = "" then None else Some (name, value)
  in
  String.split_on_char ';' s |> List.filter_map parse_pair

let to_set_cookie ?expires ?max_age ?domain ?path ?secure ?http_only ?same_site
    name value =
  let expires =
    match Option.bind expires Ptime.of_float_s with
    | None -> ""
    | Some time ->
        let day_name =
          match Ptime.weekday time with
          | `Mon -> "Mon"
          | `Tue -> "Tue"
          | `Wed -> "Wed"
          | `Thu -> "Thu"
          | `Fri -> "Fri"
          | `Sat -> "Sat"
          | `Sun -> "Sun"
        in

        let (y, m, d), ((hh, mm, ss), _tz_offset_s) = Ptime.to_date_time time in

        let month =
          match m with
          | 1 -> "Jan"
          | 2 -> "Feb"
          | 3 -> "Mar"
          | 4 -> "Apr"
          | 5 -> "May"
          | 6 -> "Jun"
          | 7 -> "Jul"
          | 8 -> "Aug"
          | 9 -> "Sep"
          | 10 -> "Oct"
          | 11 -> "Nov"
          | 12 -> "Dec"
          | _ -> assert false
        in

        Printf.sprintf "; Expires=%s, %02d %s %04d %02d:%02d:%02d GMT" day_name
          d month y hh mm ss
  in

  let max_age =
    match max_age with
    | None -> ""
    | Some seconds -> Printf.sprintf "; Max-Age=%d" seconds
  in

  let domain =
    match domain with
    | None -> ""
    | Some domain -> Printf.sprintf "; Domain=%s" domain
  in

  let path =
    match path with None -> "" | Some path -> Printf.sprintf "; Path=%s" path
  in

  let secure = match secure with Some true -> "; Secure" | _ -> "" in

  let http_only = match http_only with Some true -> "; HttpOnly" | _ -> "" in

  let same_site =
    match same_site with
    | None -> ""
    | Some `Strict -> "; SameSite=Strict"
    | Some `Lax -> "; SameSite=Lax"
    | Some `None -> "; SameSite=None"
  in

  Printf.sprintf "%s=%s%s%s%s%s%s%s%s" name value expires max_age domain path
    secure http_only same_site

let split_target s =
  let uri = Uri.of_string s in
  let path = Uri.path uri in
  let query = Uri.verbatim_query uri |> Option.value ~default:"" in
  (path, query)

let from_path s =
  if s = "" then []
  else
    let parts = String.split_on_char '/' s in
    let n = List.length parts in
    parts
    |> List.filteri (fun i part -> part <> "" || i = n - 1)
    |> List.map Uri.pct_decode

let to_path ?(relative = false) ?(international = true) parts =
  let n = List.length parts in
  let parts =
    parts
    |> List.filteri (fun i part -> part <> "" || i = n - 1)
    |> List.map (to_percent_encoded ~international)
  in

  let parts = if relative then parts else "" :: parts in
  match parts with [ "" ] -> "/" | parts -> String.concat "/" parts

let rec drop_trailing_slash = function
  | [] | [ "" ] -> []
  | part :: parts -> part :: drop_trailing_slash parts

let text_html = "text/html; charset=utf-8"
let application_json = "application/json"
