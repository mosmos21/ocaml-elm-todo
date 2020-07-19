open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback conn req body =
    eprintf "[CONN] %s\n" @@ Connection.to_string @@ snd conn;
    match Uri.path @@ Cohttp.Request.uri req with
      | uri ->
        eprintf "[URI] %s\n" uri;
        Server.respond_string ~status:`OK ~body:(sprintf "echo: %s" uri) ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = Lwt_main.run server
