exception LoadingFailed

val load_maybe_encrypted_pdf : string -> string option -> Pdf.t
(* load pdf from file and decrypt if necessary *)

val load_pages_from_maybe_encrypted_pdf : string -> string option -> Page.t list
(* load pdf from file and decrypt if necessary, then load pages with text *)
