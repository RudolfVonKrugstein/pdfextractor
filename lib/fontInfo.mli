type text_mode =
  | Fill
  | Stroke
  | FillThenStroke
  | Invisible
  | FillAndClip
  | StokeAndClip
  | FillThenStrokeAndClip
  | Clip
(* The drawing mode a text can have in a pdf *)

type t = { name : string; size : float; flags : int; mode : text_mode }

val create : Pdftext.font -> float -> text_mode -> t
(* create font description from pdf font and size *)

val to_json : t -> Yojson.t
(* convert to a json savable value *)
