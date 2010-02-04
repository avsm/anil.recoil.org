open Printf
open Cohttpserver

TYPE_CONV_PATH "Papers"

open Bib

let dispatch oc = function
 | [] ->
    let titles =
      Db.with_bib (fun db ->
        let all = ent_get db in
        let yor = function |None -> 1900 | Some x -> x in
        let all = List.sort (fun a b -> (yor b.year) - (yor a.year)) all in
        List.map ent_to_summary_html all
      ) in
    let html = String.concat "\n" (List.map (fun t ->
      "<p>" ^ t ^ "</p>") titles) in
    let body = Pages.Pages.t html in
    Http_daemon.respond ~body oc
 | _ -> Http_daemon.respond_not_found oc
