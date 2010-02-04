open Printf

let read_bibtex f =
  let ch = open_in f in
  let lb = Lexing.from_channel ch in
  try
    let el = Bibtex_parser.command_list Bibtex_lexer.token lb in
    eprintf "ok (%d entries).\n%!" (Bibtex.size el);
    el
  with
    Parsing.Parse_error | Failure "unterminated string" -> 
       (eprintf "Parse error character %d, in or after entry '%s'.\n%!" (Lexing.lexeme_start lb) !Bibtex.current_key;
       exit 1)

open Bibtex

let atom = function |Id i -> "id " ^ i |String s -> "str " ^ s
let atoms a = String.concat ", " (List.map atom a)
let output_bibtex c () =
  match c with
  |Comment c -> ()
  |Preamble al -> printf "preamble: %s\n" (atoms al)
  |Abbrev (t,al) -> printf "abbr: %s %s\n" t (atoms al)
  |Entry (ety, key, el) ->
     printf "e: %s[%s] -> %s \n" ety key
      (String.concat "\n" (List.map (fun (k,v) -> sprintf "  %s=%s" k (atoms v)) el))

let string_of_atom = function
  | Id x -> x
  | String x -> Pcre.replace ~rex:(Pcre.regexp "[{|}]") ~itempl:(Pcre.subst "") x

let normalize_author x =
  (* Blogg, Joe -> Joe Blogg *)
  let x = String.concat " " (
    List.rev (Pcre.split ~rex:(Pcre.regexp ", *") x)) in
  (* strip edge spaces *)
  let itempl = Pcre.subst "" in
  let x = Pcre.replace ~rex:(Pcre.regexp "^ +") ~itempl x in
  Pcre.replace ~rex:(Pcre.regexp " +$") ~itempl x

let string_of_atoms x =
  String.concat " " (List.map string_of_atom x)

let bib_of_bibtex = function
  | Comment _ | Preamble _ | Abbrev _ -> None
  | Entry (ety, key, el) as bt ->
      let t = Bib.t_of_string ety in
      let try_assoc k = try string_of_atoms (List.assoc k el) with Not_found -> "" in
      let authors = List.map normalize_author (Pcre.split ~pat:" and " (try_assoc "author")) in
      let year = try Some (int_of_string (try_assoc "year")) with _ -> None in
      let title = try_assoc "title" in
      let misc = List.map (fun (k,v) -> (k, (string_of_atoms v))) (List.filter (fun (k,_) -> match k with "author" |"year" |"title" -> false |_ -> true) el) in
      Some { Bib.ty=t; key=key; authors=authors; year=year; title=title; misc=misc }

let output_bibtexs cl =
  fold output_bibtex cl ()
 
let save_bibtexs bs dbname =
  let db = Bib.ent_init dbname in
  List.iter (fun c -> printf ".%!"; Bib.ent_save db c) bs

let _ =
  match Sys.argv with 
  | [| _; file; db |] ->
      let bt = read_bibtex file in
      let bso = List.map bib_of_bibtex bt in
      let bs = List.fold_left (fun a -> function None -> a | Some b -> b :: a) [] bso in
      save_bibtexs bs db
  | _ -> 
      eprintf "Usage: %s <bibtex> <db>\n%!" Sys.argv.(0)
