open Printf
open Lwt
open Cohttp
open Cohttpserver
open Log

let _ =
  printf "hello\n%!";
  Lwt_main.run ( 
    let spec = {
      Http_daemon.default_spec with
        Http_daemon.callback = Dispatch.t;
        exn_handler = Dispatch.exn_handler;
        port = Config.port;
    } in
   
    logmod "Server" "listening to HTTP on port %d" Config.port;
    Http_daemon.main spec
  )
