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
 (description "Twixt Module")
 (depends ocaml dune)
 (tags
  (topics twixt)))
