(lang dune 3.22)

(name lokto_dream)

(generate_opam_files true)

(package
 (name lokto_dream)
 (depends
  ocaml
  miou
  h1
  logs
  ptime
  mtime
  hmap
  uri
  mirage-crypto-rng
  mirage-crypto-rng-miou-unix
  lokto_xchacha20
  yojson
  (alcotest :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))

(pin
  (url "git+https://github.com/itswindtw/lokto_xchacha20")
  (package
    (name lokto_xchacha20)))
