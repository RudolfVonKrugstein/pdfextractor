exception LoadingFailed

val load_maybe_encrypted_pdf : string -> string option -> Pdf.t
(* load pdf from file and decrypt if necessary *)
