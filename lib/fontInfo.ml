type text_mode =
  | Fill
  | Stroke
  | FillThenStroke
  | Invisible
  | FillAndClip
  | StokeAndClip
  | FillThenStrokeAndClip
  | Clip

let text_mode_to_string = function
  | Fill -> "Fill"
  | Stroke -> "Stroke"
  | FillThenStroke -> "FillThenStroke"
  | Invisible -> "Invisible"
  | FillAndClip -> "FillAndClip"
  | StokeAndClip -> "StokeAndClip"
  | FillThenStrokeAndClip -> "FillThenStrokeAndClip"
  | Clip -> "Clip"

type t = { name : string; size : float; flags : int; mode : text_mode }

let create (font : Pdftext.font) size mode =
  match font with
  | StandardFont (sf, _) ->
      let name =
        match sf with
        | TimesRoman -> "TimesRoman"
        | TimesBold -> "TimesBold"
        | TimesItalic -> "TimesItalic"
        | TimesBoldItalic -> "TimesBoldItalic"
        | Helvetica -> "Helvetica"
        | HelveticaBold -> "HelveticaBold"
        | HelveticaOblique -> "HelveticaOblique"
        | HelveticaBoldOblique -> "HelveticaBoldOblique"
        | Courier -> "Courier"
        | CourierBold -> "CourierBold"
        | CourierOblique -> "CourierOblique"
        | CourierBoldOblique -> "CourierBoldOblique"
        | Symbol -> "Symbol"
        | ZapfDingbats -> "ZapfDingbats"
      in
      { name; size; flags = Pdfstandard14.flags_of_standard_font sf; mode }
  | SimpleFont sf ->
      let flags =
        match sf.fontdescriptor with None -> 0 | Some d -> d.flags
      in
      { name = sf.basefont; size; flags; mode }
  | CIDKeyedFont (_, font, _) ->
      {
        name = font.cid_basefont;
        size;
        flags = font.cid_fontdescriptor.flags;
        mode;
      }

let to_json (fi : t) =
  `Assoc
    [
      ("name", `String fi.name);
      ("size", `Float fi.size);
      ("text_mode", `String (text_mode_to_string fi.mode));
    ]
