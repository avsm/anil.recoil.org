open Printf
open Htmlgen

(* get template body *)
let get n = Db.get_tmpl n
(* substitute markdown from template *)
let md n = Markdown_html.t (Markdown.parse_text (get n))

let header = get "header.inc"
let footer = get "footer.inc"

(* substitute keys in hashtbl h in string s *)
let subst s h =
  let re frm tos s =
    let rex = Pcre.regexp frm in
    let itempl = Pcre.subst tos in
    Pcre.replace ~rex ~itempl s in
  Hashtbl.fold re h s

(* new hash with title key *)
let keys ?extra title =
  let h = Hashtbl.create 1 in
  Hashtbl.add h "@@TITLE@@" title;
  (match extra with 
  |None -> ()
  |Some l -> List.iter (fun (k,v) -> Hashtbl.add h k v) l);
  h

let page p h = subst (header ^ p ^ footer) h

module Index = struct

  let h = keys ~extra:[ "@@CONTENT@@", md "intro.md" ; "@@HEADER_PIC@@", "home" ] "index"
  let t = page (get "index.inc") h

end

module Pages = struct
  let h c = keys ~extra:[ "@@CONTENT@@", c; "@@HEADER_PIC@@", "papers" ] "papers"
  let t c = page (get "papers.inc") (h c)
end
