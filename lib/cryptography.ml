let random = Mirage_crypto_rng.generate

let sha1 s =
  Digestif.(
    s |> SHA1.digest_string |> SHA1.to_raw_string |> Base64.encode_string)

let secrets_field =
  Message.new_field ~name:"lokto_dream.secret"
    ~show_value:(fun _secrets -> "[redacted]")
    ()

let set_secret ?(old_secrets = []) secret =
  let secrets = secret :: old_secrets in
  fun next_handler request ->
    Message.set_field request secrets_field secrets;
    next_handler request

let fallback_secrets = lazy [ random 32 ]

let encryption_secret request =
  match Message.field request secrets_field with
  | Some secrets -> List.hd secrets
  | None -> List.hd (Lazy.force fallback_secrets)

let decryption_secrets request =
  match Message.field request secrets_field with
  | Some secrets -> secrets
  | None -> Lazy.force fallback_secrets

let encrypt ?(associated_data = "") request plaintext =
  let key = encryption_secret request in
  let nonce = random 24 in
  let aad = associated_data in
  let ciphertext =
    Lokto_xchacha20.authenticate_encrypt ~key ~nonce ~aad plaintext
  in
  nonce ^ ciphertext

let decrypt ?(associated_data = "") request ciphertext =
  let keys = decryption_secrets request in
  let nonce = String.sub ciphertext 0 24 in
  let aad = associated_data in
  let ciphertext = String.sub ciphertext 24 (String.length ciphertext - 24) in
  let rec go = function
    | [] -> None
    | key :: keys -> (
        match
          Lokto_xchacha20.authenticate_decrypt ~key ~nonce ~aad ciphertext
        with
        | Some plaintext -> Some plaintext
        | None -> go keys)
  in
  go keys
