type t = (float * float) * (float * float) * (float * float) * (float * float)

let from_width_and_state (text_state : TextState.t) (width : float) =
  let apply_transform p = TextState.apply_text_transform text_state p in
  let ascent, descent = TextState.font_range text_state in
  ( apply_transform (0.0, descent),
    apply_transform (width, descent),
    apply_transform (width, ascent),
    apply_transform (0.0, ascent) )

let to_json (bb : t) =
  let (x1, y1), (x2, y2), (x3, y3), (x4, y4) = bb in
  `List
    [
      `Assoc [ ("x", `Float x1); ("y", `Float y1) ];
      `Assoc [ ("x", `Float x2); ("y", `Float y2) ];
      `Assoc [ ("x", `Float x3); ("y", `Float y3) ];
      `Assoc [ ("x", `Float x4); ("y", `Float y4) ];
    ]
