type t = {
  bounds : (float * float) * (float * float);
  text : TextElement.t list;
}

let number_of_pdf (obj : Pdf.pdfobject) =
  match obj with
  | Real x -> x
  | Integer x -> float_of_int x
  | _ -> raise @@ Invalid_argument "not a pdfobject with a number"

let pdf_page_bounds (page : Pdfpage.t) =
  match page.mediabox with
  | Array [ a; b; c; d ] ->
      ((number_of_pdf a, number_of_pdf b), (number_of_pdf c, number_of_pdf d))
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
