
open Lwt
open Lwt_io

let subst_re ~frm ~tos s =
  let rex = Pcre.regexp frm in
  let itempl = Pcre.subst tos in
  Pcre.replace ~rex ~itempl s
  
let subst_url =
  subst_re ~frm:"@@URL@@" ~tos:Config.baseurl

let subst_title tos =
  subst_re ~frm:"@@TITLE@@" ~tos

let subst_content tos = 
  subst_re ~frm:"@@CONTENT@@" ~tos

let subst t body s =
  subst_title t (subst_url (subst_content body s))

let subst_file filename title body =
  with_file ~mode:input filename
    (fun ic ->
       lwt s = read ic in
       return (subst title body s)
    )

let header =
  subst_file Config.header

let footer =
  subst_file Config.footer

let tmpl =
  subst_file Config.tmpl

let t title body =
  tmpl title body
