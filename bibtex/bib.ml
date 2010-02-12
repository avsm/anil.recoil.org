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

let is_paper e = match e.ty with
  |Proceedings | Webpage |Misc -> false
  |_ -> true

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
  let key x = try List.assoc x e.misc with Not_found -> "XXX " ^ x in
  let skey x = try Some (List.assoc x e.misc) with Not_found -> None in
  let date =
     match e.year, (skey "month") with
   | None,_ -> "XXX year"
   | Some y, None -> string_of_int y
   | Some y, Some m -> sprintf "%s %d" m y in
  let links = String.concat " " (List.fold_left 
    (fun a (key,txt,icon) ->
      match skey key with
      | None -> a 
      | Some url -> (
          sprintf "<a href=\"%s\">[%s]</a> " url txt
        ) :: a
    ) [] ["url_pdf", "pdf", None; "url_html", "html", None; "doi", "doi", None ]) in
  match e.ty with
  |In_proceedings ->
     sprintf "<b>%s</b><br />%s<br /><i>%s</i>, %s<br />%s" e.title authors (key "booktitle") date links
  |Article ->
     let s = sprintf "%s" (key "journal") in
     sprintf "<b>%s</b><br />%s<br /><i>in %s </i>, %s<br />%s" e.title authors s date links
  |Phd_thesis ->
     sprintf "<b>%s</b><br />%s<br /><i>Phd Thesis, %s</i>, %s<br />%s" e.title authors (key "school") date links
  |Tech_report ->
     sprintf "<b>%s</b><br />%s<br /><i>Technical Report %s, %s</i>, %s<br />%s" e.title authors (key "number") (key "institution") date links
  |Book 
  |In_book 
  |In_collection
  |Proceedings
  |Webpage
  |Masters_thesis
  |Unpublished 
  |Misc -> sprintf "<b>%s</b>" e.title
