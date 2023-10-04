type t = (float * float) * (float * float) * (float * float) * (float * float)

let from_width_and_state (text_state : TextState.t) (width : float) =
  let apply_transform p = TextState.apply_text_transform text_state p in
  let ascent, descent = TextState.font_range text_state in
  ( apply_transform (0.0, descent),
    apply_transform (width, descent),
    apply_transform (width, ascent),
    apply_transform (0.0, ascent) )
