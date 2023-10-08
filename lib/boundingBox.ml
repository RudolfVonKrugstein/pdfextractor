type t = {
  rotated :
    (float * float) * (float * float) * (float * float) * (float * float);
  aaligned : (float * float) * (float * float);
}

let find_bb rotated =
  let max lst = List.fold_left max min_float lst in
  let min lst = List.fold_left min max_float lst in
  let (x1, y1), (x2, y2), (x3, y3), (x4, y4) = rotated in
  ( (min [ x1; x2; x3; x4 ], min [ y1; y2; y3; y4 ]),
    (max [ x1; x2; x3; x4 ], max [ y1; y2; y3; y4 ]) )

let from_width_and_state (text_state : TextState.t) (width : float) =
  let apply_transform p = TextState.apply_text_transform text_state p in
  let ascent, descent = TextState.font_range text_state in
  let rotated =
    ( apply_transform (0.0, descent),
      apply_transform (width, descent),
      apply_transform (width, ascent),
      apply_transform (0.0, ascent) )
  in
  let aaligned = find_bb rotated in
  { rotated; aaligned }

let horiz_overlap { aaligned = (a_x1, _), (a_x2, _); _ }
    { aaligned = (b_x1, _), (b_x2, _); _ } =
  a_x1 < b_x2 && b_x1 < a_x2

let vert_overlap { aaligned = (_, a_y1), (_, a_y2); _ }
    { aaligned = (_, b_y1), (_, b_y2); _ } =
  a_y1 < b_y2 && b_y1 < a_y2

let overlap a b = vert_overlap a b && horiz_overlap a b

let left_of a b =
  let { aaligned = (a_x1, _), (a_x2, _); _ } = a in
  let { aaligned = (b_x1, _), (b_x2, _); _ } = b in
  b_x1 < a_x1 && b_x2 < a_x2 && vert_overlap a b

let complete_left_of a b =
  let { aaligned = (a_x1, _), _; _ } = a in
  let { aaligned = _, (b_x2, _); _ } = b in
  b_x2 <= a_x1 && vert_overlap a b

let right_of a b =
  let { aaligned = (a_x1, _), (a_x2, _); _ } = a in
  let { aaligned = (b_x1, _), (b_x2, _); _ } = b in
  b_x1 > a_x1 && b_x2 > a_x2 && vert_overlap a b

let complete_right_of a b =
  let { aaligned = _, (a_x2, _); _ } = a in
  let { aaligned = (b_x1, _), _; _ } = b in
  b_x1 >= a_x2 && vert_overlap a b

let above a b =
  let { aaligned = (_, a_y1), (_, a_y2); _ } = a in
  let { aaligned = (_, b_y1), (_, b_y2); _ } = b in
  b_y1 < a_y1 && b_y2 < a_y2 && horiz_overlap a b

let complete_above a b =
  let { aaligned = (_, a_y1), _; _ } = a in
  let { aaligned = _, (_, b_y2); _ } = b in
  b_y2 < a_y1 && horiz_overlap a b

let below a b =
  let { aaligned = (_, a_y1), (_, a_y2); _ } = a in
  let { aaligned = (_, b_y1), (_, b_y2); _ } = b in
  b_y1 > a_y1 && b_y2 > a_y2 && horiz_overlap a b

let complete_below a b =
  let { aaligned = _, (_, a_y2); _ } = a in
  let { aaligned = (_, b_y1), _; _ } = b in
  a_y2 < b_y1 && horiz_overlap a b

let leftmost = function
  | [] -> None
  | lst ->
      Some
        (List.fold_left
           (fun a b ->
             let (a_x, _), _ = a.aaligned in
             let (b_x, _), _ = b.aaligned in
             if a_x < b_x then a else b)
           (List.hd lst) lst)

let rightmost = function
  | [] -> None
  | lst ->
      Some
        (List.fold_left
           (fun a b ->
             let (a_x, _), _ = a.aaligned in
             let (b_x, _), _ = b.aaligned in
             if a_x > b_x then a else b)
           (List.hd lst) lst)

let top = function
  | [] -> None
  | lst ->
      Some
        (List.fold_left
           (fun a b ->
             let (_, a_y), _ = a.aaligned in
             let (_, b_y), _ = b.aaligned in
             if a_y < b_y then a else b)
           (List.hd lst) lst)

let bottom = function
  | [] -> None
  | lst ->
      Some
        (List.fold_left
           (fun a b ->
             let (_, a_y), _ = a.aaligned in
             let (_, b_y), _ = b.aaligned in
             if a_y < b_y then a else b)
           (List.hd lst) lst)

let to_json (bb : t) =
  let (x1, y1), (x2, y2), (x3, y3), (x4, y4) = bb.rotated in
  let (aa_x1, aa_y1), (aa_x2, aa_y2) = bb.aaligned in
  `Assoc
    [
      ( "roatated",
        `List
          [
            `Assoc [ ("x", `Float x1); ("y", `Float y1) ];
            `Assoc [ ("x", `Float x2); ("y", `Float y2) ];
            `Assoc [ ("x", `Float x3); ("y", `Float y3) ];
            `Assoc [ ("x", `Float x4); ("y", `Float y4) ];
          ] );
      ( "aaligned",
        `List
          [
            `Assoc [ ("x", `Float aa_x1); ("y", `Float aa_y1) ];
            `Assoc [ ("x", `Float aa_x2); ("y", `Float aa_y2) ];
          ] );
    ]
