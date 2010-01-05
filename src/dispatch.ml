open Printf
open Cohttp
open Cohttpserver
open Log
open Lwt

module Resp = struct
  
  (* dispatch non-file URLs *)
  let dispatch req oc = function
    | [] 
    | "index.html" :: [] ->
        let body = Pages.Index.t in
        Http_daemon.respond ~body oc
    | _ -> 
      Http_daemon.respond_not_found oc

end

(* handle exceptions with a 500 *)
let exn_handler exn oc =
  let status = `Status (`Server_error `Internal_server_error) in
  let body = sprintf "Internal error: %s" (Printexc.to_string exn) in
  Http_daemon.respond_error ~body ~status oc

(* main callback function *)
let t req oc =
  let path = Http_request.path req in

  logmod "HTTP" "%s %s [%s]" (Http_common.string_of_method (Http_request.meth req)) path 
    (String.concat "," (List.map (fun (h,v) -> sprintf "%s=%s" h v) 
      (Http_request.params_get req)));

  (* normalize path to strip out ../. and such *)
  let path_elem = List.filter (fun x -> x <> "") (Neturl.norm_path (Pcre.split ~pat:"/" path)) in

  logmod "File" "%s" (String.concat "/" path_elem);
  (* determine if it is static or dynamic content *)
  match Static.Files.t (String.concat "/" path_elem) with 
  | Some body ->
      let status = `Status (`Success `OK) in
      Http_daemon.respond ~body ~status oc
  | None -> Resp.dispatch req oc path_elem
