type t = {
  rotated :
    (float * float) * (float * float) * (float * float) * (float * float);
  aaligned : (float * float) * (float * float);
}
(* real bound box (roated by transformation) as well as axis aligned bounding box *)

(* val vert_distance : t -> t -> float *)
val vert_overlap : t -> t -> bool
val overlap : t -> t -> bool
val left_of : t -> t -> bool
val complete_left_of : t -> t -> bool
val right_of : t -> t -> bool
val complete_right_of : t -> t -> bool
val above : t -> t -> bool
val complete_above : t -> t -> bool
val below : t -> t -> bool
val complete_below : t -> t -> bool
val from_width_and_state : TextState.t -> float -> t
val to_json : t -> Yojson.t
val leftmost : t list -> t option
val rightmost : t list -> t option
val top : t list -> t option
val bottom : t list -> t option
