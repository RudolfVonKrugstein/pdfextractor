exception LoadingFailed

let load_maybe_encrypted_pdf file_name password =
  let pdf = Pdfread.pdf_of_file None None file_name in
  if Pdfcrypt.is_encrypted pdf then
    match
      Pdfcrypt.decrypt_pdf_owner (Option.value ~default:"" password) pdf
    with
    | None -> raise @@ LoadingFailed
    | Some pdf -> pdf
  else pdf

let load_pages_from_maybe_encrypted_pdf file_name password =
  let pdf = load_maybe_encrypted_pdf file_name password in
  List.map (Page.from_pdf_page pdf) (Pdfpage.pages_of_pagetree pdf)
