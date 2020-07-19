open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix

let dbd = Mysql.connect 
  ~options:[Mysql.OPT_PROTOCOL Mysql.PROTOCOL_TCP] 
  { dbhost   = Some "localhost"
  ;	dbname   = Some "todo_app"
  ; dbport   = Some 3306
  ; dbpwd    = Some "password"
  ; dbuser   = Some "todo_user"
  ;	dbsocket = None
  }

let read_todo () =
  let result = Mysql.(exec dbd "select * from todo_app.todo") in
  let col = Mysql.column result in
  let row x = Mysql.(
    ( not_null int2ml (col ~key:"id" ~row: x)
    , not_null str2ml (col ~key:"title" ~row: x)
    , not_null str2ml (col ~key:"content" ~row: x)
    )
  ) 
  in
  let rec loop = function
    | None -> []
    | Some x -> row x :: loop (Mysql.fetch result)
  in
  loop (Mysql.fetch result)

let string_of_todo (id, title, content) = sprintf "%d: [%s] %s" id title content

let server =
  let callback conn req body =
    eprintf "[CONN] %s\n" @@ Connection.to_string @@ snd conn;
    match Uri.path @@ Cohttp.Request.uri req with
      | uri ->
        let todos = List.map string_of_todo (read_todo ()) |> String.concat "\n" in
        Server.respond_string ~status:`OK ~body:todos ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = Lwt_main.run server