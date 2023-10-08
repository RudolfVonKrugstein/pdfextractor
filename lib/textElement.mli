type t = {
  font_info : FontInfo.t;
  text : string;
  width : float;
  bounding_box : BoundingBox.t;
}

val list_from_pdf_ops : FontCache.t -> Pdfops.t list -> t list
val to_json : t -> Yojson.t
