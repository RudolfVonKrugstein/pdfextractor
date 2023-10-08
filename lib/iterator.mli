type t = { text : TextElement.t; page : Page.t }

val find_first : Page.t -> ?unique:bool -> (TextElement.t -> bool) -> t option

val find_first_in_bb_by_regex :
  Page.t -> ?unique:bool -> BoundingBox.t -> Str.regexp -> t option

val find_first_in_percentage_bb_by_regex :
  Page.t ->
  ?unique:bool ->
  (float * float) * (float * float) ->
  Str.regexp ->
  t option

val find_all : Page.t -> (TextElement.t -> bool) -> t list
val find_all_in_bb_by_regex : Page.t -> BoundingBox.t -> Str.regexp -> t list

val find_all_in_percentage_bb_by_regex :
  Page.t -> (float * float) * (float * float) -> Str.regexp -> t list

val move_left : ?no_overlap:bool -> ?f:(TextElement.t -> bool) -> t -> t option
val move_right : ?no_overlap:bool -> ?f:(TextElement.t -> bool) -> t -> t option
val move_up : ?no_overlap:bool -> ?f:(TextElement.t -> bool) -> t -> t option
val move_down : ?no_overlap:bool -> ?f:(TextElement.t -> bool) -> t -> t option
