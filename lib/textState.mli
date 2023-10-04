type t
(* State for drawing text in pdf *)

val initial : t
(* Initial text state when starting to parse pdf*)

val reset_transforms : t -> t
(* Reset all transformation matrices *)

val advance : t -> float -> t
(* advance the position in typing direction *)

val translate : t -> float -> float -> t
(* Translate the position of the text *)

val with_transform : t -> Pdftransform.transform_matrix -> t
(* set the transformation matrix*)

val newline : t -> t
(* go to the next line*)

val get_text_and_advance : t -> Pdf.pdfobject -> string * float
(* Get the text and advancement distance from an pdf object containing text*)

val with_charspace : t -> float -> t
val with_wordspace : t -> float -> t
val with_leading : t -> float -> t
val with_font : t -> Pdftext.font option -> float -> t
val with_text_mode : t -> int -> t
val with_horizontal_scale : t -> float -> t
val with_rise : t -> float -> t
val font_info : t -> FontInfo.t option
val font_range : t -> float * float
(* the range of the font vertically *)

val apply_text_transform : t -> float * float -> float * float
