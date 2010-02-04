open Printf

type t =
  |Book 
  |In_proceedings 
  |In_book 
  |In_collection
  |Article 
  |Proceedings
  |Webpage
  |Tech_report 
  |Phd_thesis
  |Masters_thesis
  |Unpublished 
  |Misc
and ent = {
  ty: t;
  key: string;
  authors: string list;
  year: int option;
  title: string;
  misc: (string * string) list
} with orm

let t_of_string = function
  |"book" -> Book 
  |"inproceedings" -> In_proceedings 
  |"article" -> Article
  |"proceedings" -> Proceedings 
  |"webpage" -> Webpage 
  |"techreport" -> Tech_report
  |"phdthesis" -> Phd_thesis
  |"mastersthesis" -> Masters_thesis
  |"unpublished" -> Unpublished 
  |"misc" -> Misc
  |"inbook" -> In_book 
  |"incollection" -> In_collection
  |x -> failwith ("unknown bibtex type: " ^ x)

let author_html a = a

let ent_to_summary_html e =
  let authors = match List.rev e.authors with
   | [] -> "XXX author"
   | [a] -> a
   | a :: tl ->
      String.concat ", " (List.map author_html (List.rev tl)) ^ " and " ^ (author_html a) in
  let year = match e.year with 
   | None -> "XXX year"
   | Some y -> string_of_int y in
  let key x = try List.assoc x e.misc with Not_found -> "XXX " ^ x in
  match e.ty with
  |In_proceedings ->
     sprintf "<b>%s</b>, %s, <i>%s</i>" e.title authors (key "booktitle")
  |Article 
  |Book 
  |In_book 
  |In_collection
  |Proceedings
  |Webpage
  |Tech_report 
  |Phd_thesis
  |Masters_thesis
  |Unpublished 
  |Misc -> sprintf "<b>%s</b>" e.title
