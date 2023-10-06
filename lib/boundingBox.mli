type t = (float * float) * (float * float) * (float * float) * (float * float)

val from_width_and_state : TextState.t -> float -> t
val to_json : t -> Yojson.t
