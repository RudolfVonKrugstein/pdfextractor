type t = {
  bounds : (float * float) * (float * float);
  text : TextElement.t list;
}

val from_pdf_page : Pdf.t -> Pdfpage.t -> t
val to_json : t -> Yojson.t
