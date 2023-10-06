module IntMap = Map.Make (Int)

type t = {
  text_extractor : Pdftext.text_extractor;
  text_width : int list -> string -> float;
}

(* use this char width if nothing else is found*)
let fallback_char_width = 400.0

(* invert a hashtable of type (int->string) to (int->int) so we can lookup the keys form the codepoints *)
let to_codepoints_table table =
  let inverted_table = Hashtbl.create (Hashtbl.length table) in
  Hashtbl.iter
    (fun key value ->
      let codepoints = Pdftext.codepoints_of_utf8 value in
      Hashtbl.add inverted_table (List.hd (List.tl codepoints)) key)
    table;
  inverted_table

let get_simple_font_char_width (simple_font : Pdftext.simple_font) index =
  (* try via the 'width' member *)
  try float_of_int @@ simple_font.widths.(index - simple_font.firstchar)
  with Invalid_argument _ -> (
    match simple_font.fontmetrics with
    | None -> fallback_char_width
    | Some metrics -> (
        (* try to get the width from the matrics *)
        try metrics.(index)
        with (* nothing found, use fallback *)
        | Invalid_argument _ ->
          fallback_char_width))

let get_cid_font_char_width (cid_font : Pdftext.composite_CIDfont) index =
  let width_map = IntMap.of_list cid_font.cid_widths in
  Option.value ~default:fallback_char_width @@ IntMap.find_opt index width_map

let get_codepoint_to_width_index_fun font =
  match font with
  | Pdftext.StandardFont _ -> fun i -> i
  | Pdftext.SimpleFont
      { fontdescriptor = Some { tounicode = Some tounicode; _ }; _ } ->
      let table = to_codepoints_table tounicode in
      fun i -> Option.value ~default:i (Hashtbl.find_opt table i)
  | CIDKeyedFont
      (_, { cid_fontdescriptor = { tounicode = Some tounicode; _ }; _ }, _) ->
      let table = to_codepoints_table tounicode in
      fun i -> Option.value ~default:i (Hashtbl.find_opt table i)
  | _ -> fun i -> i

let create font font_size =
  let ctwi = get_codepoint_to_width_index_fun font in
  let text_width =
    match font with
    | Pdftext.StandardFont (sf, ec) ->
        let tw _ text =
          let milli_points = Pdfstandard14.textwidth false ec sf text in
          float_of_int milli_points *. font_size /. 1000.0
        in
        tw
    | Pdftext.SimpleFont simple_font ->
        let tw codes _ =
          let milli_points =
            List.fold_left ( +. ) 0.0
            @@ List.map
                 (fun i -> get_simple_font_char_width simple_font @@ ctwi i)
                 codes
          in
          milli_points *. font_size /. 1000.0
        in
        tw
    | Pdftext.CIDKeyedFont (_, cid_font, _) ->
        let tw codes _ =
          let milli_points =
            List.fold_left ( +. ) 0.
            @@ List.map
                 (fun i -> get_cid_font_char_width cid_font @@ ctwi i)
                 codes
          in
          milli_points *. font_size /. 1000.0
        in
        tw
  in
  { text_extractor = Pdftext.text_extractor_of_font_real font; text_width }

let get_text_and_width_of_string t char_space word_space str =
  let codes = Pdftext.codepoints_of_text t.text_extractor str in
  let words =
    float_of_int @@ List.length @@ List.filter (fun i -> i == 32) codes
  in
  let chars = float_of_int @@ List.length codes in
  ( Pdftext.utf8_of_codepoints codes,
    (chars *. char_space) +. (words *. word_space) +. t.text_width codes str )

let rec get_text_and_width_of_obj_list t char_space word_space
    (objs : Pdf.pdfobject list) =
  match objs with
  | [] -> ("", 0.0)
  | obj :: objs ->
      let f_t, f_w =
        match obj with
        | String s -> get_text_and_width_of_string t char_space word_space s
        | Real r -> ("", r /. -1000.0)
        | _ -> raise @@ Invalid_argument "cannot get text from this pdfobject"
      in
      let rest_t, rest_w =
        get_text_and_width_of_obj_list t char_space word_space objs
      in
      (f_t ^ rest_t, f_w +. rest_w)

let get_text_and_width t char_space word_space (obj : Pdf.pdfobject) =
  match obj with
  | String s -> get_text_and_width_of_string t char_space word_space s
  | Array a -> get_text_and_width_of_obj_list t char_space word_space a
  | _ -> raise @@ Invalid_argument "cannot get text from this pdfobject"
