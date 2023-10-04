type t
(** Cache of fonts, extracted from pdf *)

val load : Pdf.t -> Pdfpage.t -> t
(** Load font cache for pdf page *)

val font : t -> string -> Pdftext.font option
(** Get a font from the Cache **)
