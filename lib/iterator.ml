type t = { text : TextElement.t; page : Page.t }

let find_all (page : Page.t) filter =
  List.map (fun text -> { text; page }) @@ List.filter filter page.text

let find_all_in_bb_by_regex (page : Page.t) bb regex =
  find_all page (fun t ->
      BoundingBox.overlap t.bounding_box bb && Str.string_match regex t.text 0)

let percent_pos min max p = min +. ((max -. min) *. p)

let find_all_in_percentage_bb_by_regex (page : Page.t)
    ((px_min, py_min), (px_max, py_max)) regex =
  let (page_x_min, page_y_min), (page_x_max, page_y_max) = page.bounds in
  let percent_x = percent_pos page_x_min page_x_max in
  let percent_y = percent_pos page_y_min page_y_max in

  let bb : BoundingBox.t =
    {
      aaligned =
        ( (percent_x px_min, percent_y py_min),
          (percent_x px_max, percent_y py_max) );
      rotated = ((0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0));
    }
  in
  find_all_in_bb_by_regex page bb regex

let rec find_first_non_unique (page : Page.t) rest filter =
  match rest with
  | [] -> None
  | a :: _ when filter a -> Some { text = a; page }
  | _ :: l -> find_first_non_unique page l filter

let find_first (page : Page.t) ?(unique = false) filter =
  if unique then
    let all = find_all page filter in
    match all with [ i ] -> Some i | _ -> None
  else find_first_non_unique page page.text filter

let find_first_in_bb_by_regex (page : Page.t) ?(unique = false) bb regex =
  find_first page ~unique (fun t ->
      BoundingBox.overlap t.bounding_box bb && Str.string_match regex t.text 0)

let find_first_in_percentage_bb_by_regex (page : Page.t) ?(unique = false)
    ((px_min, py_min), (px_max, py_max)) regex =
  let (page_x_min, page_y_min), (page_x_max, page_y_max) = page.bounds in
  let percent_x = percent_pos page_x_min page_x_max in
  let percent_y = percent_pos page_y_min page_y_max in

  let bb : BoundingBox.t =
    {
      aaligned =
        ( (percent_x px_min, percent_y py_min),
          (percent_x px_max, percent_y py_max) );
      rotated = ((0.0, 0.0), (0.0, 0.0), (0.0, 0.0), (0.0, 0.0));
    }
  in
  find_first_in_bb_by_regex page ~unique bb regex

let rec extreme_pos (cmp : BoundingBox.t -> BoundingBox.t -> bool)
    (candidates : t list) (winner : t option) =
  match candidates with
  | [] -> winner
  | c :: cs ->
      let next_winner =
        match winner with
        | None -> Some c
        | Some w when cmp c.text.bounding_box w.text.bounding_box -> Some c
        | Some w -> Some w
      in
      extreme_pos cmp cs next_winner

let leftmost candidates =
  extreme_pos (fun a b -> BoundingBox.more_left b a) candidates None

let rightmost candidates =
  extreme_pos (fun a b -> BoundingBox.more_right b a) candidates None

let topmost candidates =
  extreme_pos (fun a b -> BoundingBox.higher b a) candidates None

let bottommost candidates =
  extreme_pos (fun a b -> BoundingBox.lower b a) candidates None

let move_left ?(no_overlap = false) ?(f = fun _ -> true) i =
  let candidates =
    find_all i.page (fun t ->
        (if no_overlap then
           BoundingBox.complete_left_of i.text.bounding_box t.bounding_box
         else BoundingBox.left_of i.text.bounding_box t.bounding_box)
        && f t)
  in
  rightmost candidates

let move_right ?(no_overlap = false) ?(f = fun _ -> true) i =
  let candidates =
    find_all i.page (fun t ->
        (if no_overlap then
           BoundingBox.complete_right_of i.text.bounding_box t.bounding_box
         else BoundingBox.right_of i.text.bounding_box t.bounding_box)
        && f t)
  in
  leftmost candidates

let move_up ?(no_overlap = false) ?(f = fun _ -> true) i =
  let candidates =
    find_all i.page (fun t ->
        (if no_overlap then
           BoundingBox.complete_above i.text.bounding_box t.bounding_box
         else BoundingBox.above i.text.bounding_box t.bounding_box)
        && f t)
  in
  bottommost candidates

let move_down ?(no_overlap = false) ?(f = fun _ -> true) i =
  let candidates =
    find_all i.page (fun t ->
        (if no_overlap then
           BoundingBox.complete_below i.text.bounding_box t.bounding_box
         else BoundingBox.below i.text.bounding_box t.bounding_box)
        && f t)
  in
  topmost candidates
