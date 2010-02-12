open Printf
open Cohttpserver

TYPE_CONV_PATH "Papers"

open Bib

let dispatch oc = function
 | [] ->
    let h = Hashtbl.create 1 in
    Db.with_bib (fun db ->
      let all = List.filter is_paper (ent_get db) in
      let yor = function |None -> 1900 | Some x -> x in
      List.iter (fun p -> 
        let yr = yor p.year in
        try
          let c = Hashtbl.find h yr in
          Hashtbl.replace h yr (p :: c)
        with Not_found ->
          Hashtbl.add h (yor p.year) [p]
       ) all;
    );
    let yrs = List.sort (fun a b -> b - a) (Hashtbl.fold (fun k v a -> k :: a) h []) in
    let ls =  List.map (fun yr ->
       let fst = ref true in
       String.concat "\n" (List.map (fun t ->
         (match !fst with
         |true -> fst := false; sprintf "<div class=\"paper_year\">%d</div>" yr
         |false -> "<div class=\"paper_year\"></div>") ^
         "<div class=\"paper_entry\">" ^ (Bib.ent_to_summary_html t) ^ "</div>") (Hashtbl.find h yr))) yrs in
    let html = String.concat "\n" ls in
    let body = Pages.Pages.t html in
    Http_daemon.respond ~body oc
 | _ -> Http_daemon.respond_not_found oc
