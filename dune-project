(lang dune 3.15)

(name ocaml_twixt_lib)

(generate_opam_files true)

(source
 (github ocaml_twixt/api))

(authors "loockeeer" "TheoFj")

(maintainers "loockeeer" "TheoFj")

(license LICENSE)

(documentation https://github.com/ocaml_twixt)

(package
 (name ocaml_twixt_lib)
 (synopsis "Ocaml Twixt Library")
 (depends ocaml dune))

(package
 (name ocaml_twixt_exchange)
 (synopsis "Ocaml Twixt Exchange")
 (depends ocaml dune))

(package
 (name ocaml_twixt_api)
 (synopsis "Ocaml Twixt API")
 (depends ocaml dune))
