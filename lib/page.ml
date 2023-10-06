type t = {
  bounds : (float * float) * (float * float);
  text : TextElement.t list;
}

let pdf_page_bounds (page : Pdfpage.t) =
  match page.mediabox with
  | Array [ Real x; Real y; Real w; Real h ] -> ((x, y), (w, h))
  | Array [ Integer x; Integer y; Integer w; Integer h ] ->
      ((float_of_int x, float_of_int y), (float_of_int w, float_of_int h))
  | _ -> raise @@ Invalid_argument "not a mediabox"

let from_pdf_page pdf page =
  let bounds = pdf_page_bounds page in
  let fc_cache = FontCache.load pdf page in
  let operators = Pdfops.parse_operators pdf page.resources page.content in
  let text = TextElement.list_from_pdf_ops fc_cache operators in
  { bounds; text }

let to_json page =
  let (x, y), (w, h) = page.bounds in
  `Assoc
    [
      ( "bounding_box",
        `Assoc
          [
            ("x", `Float x);
            ("y", `Float y);
            ("w", `Float w);
            ("h", `Float h);
            ("text", `List (List.map TextElement.to_json page.text));
          ] );
    ]
