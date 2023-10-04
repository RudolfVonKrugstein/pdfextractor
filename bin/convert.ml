open Pdfextractor
module StringMap = Map.Make (String)

let print_page index (page : Page.t) =
  Printf.printf "--------------------\nPAGE %i\n--------------------" index;
  let (x, y), (w, h) = page.bounds in
  Printf.printf "bounds: %f, %f + %f, %f\n" x y w h;
  let surface =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:(int_of_float w)
      ~h:(int_of_float h)
  in
  let cr = Cairo.create surface in
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.rectangle cr 0.0 0.0 ~w ~h;
  Cairo.fill cr;
  Cairo.set_line_width cr 1.0;
  Cairo.set_source_rgb cr 0. 0. 0.;
  List.iter
    (fun (t : TextElement.t) ->
      let (x1, y1), (x2, y2), (x3, y3), (x4, y4) = t.bounding_box in
      Cairo.move_to cr x1 (h -. y1);
      Cairo.line_to cr x2 (h -. y2);
      Cairo.line_to cr x3 (h -. y3);
      Cairo.line_to cr x4 (h -. y4);
      Cairo.line_to cr x1 (h -. y1);
      Cairo.stroke cr;
      Cairo.move_to cr x1 (h -. y1);
      Cairo.show_text cr t.text)
    page.text;
  Cairo.PNG.write surface @@ Printf.sprintf "page%i.png" index

(* for command line arguments *)
let usage_msg = "convert <pdf>"
let pdf_file = ref ""
let anon_fun fname = pdf_file := fname

let () =
  (* read command line arguments *)
  Arg.parse [] anon_fun usage_msg;
  let pdf =
    match pdf_file.contents with
    | "" -> raise @@ Invalid_argument "missing pdf file argument"
    | s -> PdfLoader.load_maybe_encrypted_pdf s None
  in
  Printf.printf "encrypted: %b" @@ Pdfcrypt.is_encrypted pdf;
  let pages =
    List.map (Page.from_pdf_page pdf) @@ Pdfpage.pages_of_pagetree pdf
  in
  List.iteri print_page pages
