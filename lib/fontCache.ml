open Pdfpage
module StringMap = Map.Make (String)

type t = { cache : Pdftext.font StringMap.t }

let load pdf page =
  (* add another font to the cache *)
  let add_font_to_map m (name, pdf_font) =
    match Pdf.lookup_direct pdf "/Type" pdf_font with
    (* '/Type' is '/Font', load and add it *)
    | Some (Pdf.Name "/Font") ->
        let font = Pdftext.read_font pdf pdf_font in
        StringMap.add name font m
        (* Does not seem to be a font, don't add anything *)
    | _ -> m
  in
  match Pdf.lookup_direct pdf "/Font" page.resources with
  | Some (Pdf.Dictionary font_resources) ->
      let new_cache =
        List.fold_left add_font_to_map StringMap.empty font_resources
      in
      { cache = new_cache }
  (* No fonts found, empty cache! *)
  | _ -> { cache = StringMap.empty }

let font fc name = StringMap.find_opt name fc.cache
