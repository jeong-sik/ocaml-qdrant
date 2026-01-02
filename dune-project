(lang dune 3.0)
(name qdrant)
(version 0.1.0)

(generate_opam_files true)

(source (github jeong-sik/ocaml-qdrant))
(license MIT)
(authors "yousleepwhen <yousleepwhen@gmail.com>")
(maintainers "yousleepwhen <yousleepwhen@gmail.com>")

(package
 (name qdrant)
 (synopsis "OCaml client for Qdrant vector database")
 (description "Pure OCaml client for Qdrant REST API. Supports vector search, collection management, and point operations.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (lwt (>= 5.6))
  (lwt_ppx (>= 2.1))
  (cohttp-lwt-unix (>= 5.0))
  (yojson (>= 2.0))
  (uri (>= 4.0))
  (alcotest (and :with-test (>= 1.7)))
  (odoc :with-doc)))
