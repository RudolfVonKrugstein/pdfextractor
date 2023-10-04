type t

val create : Pdftext.font -> float -> t
(* Create the extractor *)

val get_text_and_width : t -> float -> float -> Pdf.pdfobject -> string * float
(* Get the text and the width from an pdf object *)
