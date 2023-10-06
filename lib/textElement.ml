type t = {
  font_info : FontInfo.t option;
  text : string;
  width : float;
  bounding_box : BoundingBox.t;
}

let rec from_pdf_ops_rec fc text_state (ops : Pdfops.t list) =
  match ops with
  | [] -> []
  | op :: ops -> (
      let next_state, new_text =
        match op with
        | Op_BT -> (TextState.reset_transforms text_state, None)
        | Op_ET -> (text_state, None)
        | Op_Tc s -> (TextState.with_charspace text_state s, None)
        | Op_Tw s -> (TextState.with_wordspace text_state s, None)
        | Op_TL l -> (TextState.with_leading text_state l, None)
        | Op_Tz s ->
            (TextState.with_horizontal_scale text_state (s /. 100.0), None)
        | Op_Ts r -> (TextState.with_rise text_state r, None)
        | Op_Tf (name, size) ->
            (TextState.with_font text_state (FontCache.font fc name) size, None)
        | Op_Tr i -> (TextState.with_text_mode text_state i, None)
        | Op_Td (x, y) -> (TextState.translate text_state x y, None)
        | Op_TD (x, y) ->
            let text_state = TextState.with_leading text_state (-.y) in
            (TextState.translate text_state x y, None)
        | Op_T' -> (TextState.newline text_state, None)
        | Op_Tm new_m -> (TextState.with_transform text_state new_m, None)
        | Op_' s ->
            (* newline + Tj *)
            let text_state = TextState.newline text_state in
            let text, advance =
              TextState.get_text_and_advance text_state (Pdf.String s)
            in
            (TextState.advance text_state advance, Some (text, advance))
        | Op_'' (ws, cs, s) ->
            (* newline + Tj *)
            let text_state = TextState.with_wordspace text_state ws in
            let text_state = TextState.with_charspace text_state cs in
            let text, advance =
              TextState.get_text_and_advance text_state (Pdf.String s)
            in
            (TextState.advance text_state advance, Some (text, advance))
        | Op_Tj s ->
            let text, advance =
              TextState.get_text_and_advance text_state (Pdf.String s)
            in
            (TextState.advance text_state advance, Some (text, advance))
        | Op_TJ obj ->
            let text, advance = TextState.get_text_and_advance text_state obj in
            (TextState.advance text_state advance, Some (text, advance))
        | _ -> (text_state, None)
      in
      (* extend the list if new text was 'created' by this op *)
      match new_text with
      | Some (text, width) ->
          let bb = BoundingBox.from_width_and_state text_state width in
          (* new element *)
          {
            text;
            font_info = TextState.font_info text_state;
            width;
            bounding_box = bb;
          }
          :: from_pdf_ops_rec fc next_state ops
      | None ->
          from_pdf_ops_rec fc next_state ops (* no new element, continue *))

let list_from_pdf_ops fc ops = from_pdf_ops_rec fc TextState.initial ops

let to_json (te : t) =
  `Assoc
    [
      ("text", `String te.text);
      ("bounding_box", BoundingBox.to_json te.bounding_box);
      ( "font",
        match te.font_info with
        | Some f -> FontInfo.to_json f
        | None -> `String "no font" );
    ]
