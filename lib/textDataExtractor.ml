module IntMap = Map.Make (Int)

type t = {
  text_extractor : Pdftext.text_extractor;
  text_width : int list -> string -> float;
}

let create font font_size =
  let text_width =
    match font with
    | Pdftext.StandardFont (sf, ec) ->
        let tw _ text =
          let milli_points = Pdfstandard14.textwidth false ec sf text in
          float_of_int milli_points *. font_size /. 1000.0
        in
        tw
    | Pdftext.SimpleFont { widths = w; firstchar = fc; _ } ->
        let tw codes _ =
          let milli_points =
            List.fold_left ( + ) 0
            @@ List.map
                 (fun c -> try w.(c - fc) with Invalid_argument _ -> 500)
                 codes
          in
          float_of_int milli_points *. font_size /. 1000.0
        in
        tw
    | Pdftext.CIDKeyedFont (_, { cid_widths = w; _ }, _) ->
        let w_map = IntMap.of_list w in
        let tw codes _ =
          let milli_points =
            List.fold_left ( +. ) 0.
            @@ List.map
                 (fun i -> Option.value ~default:0.0 @@ IntMap.find_opt i w_map)
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
