type t = {
  text_matrix : Pdftransform.transform_matrix;
  line_matrix : Pdftransform.transform_matrix;
  char_space : float;
  word_space : float;
  horizontal_scale : float;
  leading : float;
  font : Pdftext.font option;
  text_extractor : TextDataExtractor.t option;
  font_size : float;
  rise : float;
  text_mode : FontInfo.text_mode;
}

let initial =
  {
    text_matrix = Pdftransform.i_matrix;
    line_matrix = Pdftransform.i_matrix;
    char_space = 0.0;
    word_space = 0.0;
    horizontal_scale = 1.0;
    leading = 1.0;
    font = None;
    text_extractor = None;
    font_size = 1.0;
    rise = 0.0;
    text_mode = Fill;
  }

let with_charspace text_state char_space = { text_state with char_space }
let with_wordspace text_state word_space = { text_state with word_space }
let with_leading text_state leading = { text_state with leading }

let with_font text_state font font_size =
  {
    text_state with
    font;
    font_size;
    text_extractor =
      Option.map (fun font -> TextDataExtractor.create font font_size) font;
  }

let with_text_mode text_state i =
  match i with
  | 0 -> { text_state with text_mode = Fill }
  | 1 -> { text_state with text_mode = Stroke }
  | 2 -> { text_state with text_mode = FillThenStroke }
  | 3 -> { text_state with text_mode = Invisible }
  | 4 -> { text_state with text_mode = FillAndClip }
  | 5 -> { text_state with text_mode = StokeAndClip }
  | 6 -> { text_state with text_mode = FillThenStrokeAndClip }
  | 7 -> { text_state with text_mode = Clip }
  | _ -> text_state

let with_horizontal_scale text_state horizontal_scale =
  { text_state with horizontal_scale }

let with_rise text_state rise = { text_state with rise }

let reset_transforms text_state =
  {
    text_state with
    text_matrix = Pdftransform.i_matrix;
    line_matrix = Pdftransform.i_matrix;
  }

let translate text_state x y =
  let new_m =
    Pdftransform.matrix_compose text_state.line_matrix
      (Pdftransform.mktranslate x y)
  in
  { text_state with text_matrix = new_m; line_matrix = new_m }

let advance text_state dist =
  let new_m =
    Pdftransform.matrix_compose text_state.text_matrix
      (Pdftransform.mktranslate dist 0.0)
  in
  { text_state with text_matrix = new_m }

let with_transform text_state new_m =
  { text_state with text_matrix = new_m; line_matrix = new_m }

let newline text_state = translate text_state 0.0 (-.text_state.leading)

let get_text_and_advance text_state (obj : Pdf.pdfobject) =
  match text_state.text_extractor with
  | None -> raise @@ Invalid_argument "no font is set"
  | Some e ->
      let text, width =
        TextDataExtractor.get_text_and_width e text_state.char_space
          text_state.word_space obj
      in
      (text, width *. text_state.horizontal_scale)

let font_info text_state =
  Option.map
    (fun f -> FontInfo.create f text_state.font_size text_state.text_mode)
    text_state.font

let font_range text_state =
  (* get the range of the font in point/1000.0 *)
  let milli_acent, milli_descent =
    match text_state.font with
    | Some (Pdftext.StandardFont (sf, _)) ->
        let headers, _, _, _ = Pdfstandard14.afm_data sf in
        ( float_of_string @@ Hashtbl.find headers "Ascent",
          float_of_string @@ Hashtbl.find headers "Descent" )
    | Some (Pdftext.SimpleFont { fontdescriptor = fc; _ }) -> (
        match fc with
        | None -> raise @@ Invalid_argument "no font selected"
        | Some fc -> (fc.ascent, fc.descent))
    | Some (Pdftext.CIDKeyedFont (_, { cid_fontdescriptor = fc; _ }, _)) ->
        (fc.ascent, fc.descent)
    | _ -> (1000.0, 0.0)
  in
  if milli_descent != milli_descent then
    ( (text_state.font_size *. milli_acent /. 1000.0) +. text_state.rise,
      (text_state.font_size *. milli_descent /. 1000.0) +. text_state.rise )
  else (1.0, 0.0)

let apply_text_transform text_state (x, y) =
  Pdftransform.transform_matrix text_state.text_matrix (x, y)
