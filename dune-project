(lang dune 3.10)

(name pdfextractor)

(generate_opam_files true)

(source
 (github RudolfVonKrugstein/pdfextractor))

(authors "Nathan H\195\188sken")

(maintainers "Nathan H\195\188sken")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name pdfextractor)
 (synopsis "Extract text from pdfs")
 (description "Extract text and positioning information from pdfs")
 (depends ocaml dune camlpdf cairo2 yojson))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
