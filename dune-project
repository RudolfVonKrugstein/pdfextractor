(lang dune 3.10)

(name pdfextractor)

(generate_opam_files true)

(source
 (github RudolfVonKrugstein/pdfextractor))

(authors "Nathan Hüsken")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name pdfextractor)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune camlpdf cairo2)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
