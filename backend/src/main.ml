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

let parse_todo col x = 
  Mysql.(
    ( not_null int2ml (col ~key:"id" ~row: x)
    , not_null str2ml (col ~key:"title" ~row: x)
    , not_null str2ml (col ~key:"content" ~row: x)
    , not_null int2ml (col ~key:"status" ~row: x)
    , not_null str2ml (col ~key:"created_at" ~row: x)
    , not_null str2ml (col ~key:"updated_at" ~row: x)
    )
  )

let read_todos () =
  let result = Mysql.(exec dbd "select * from todo_app.todo") in
  let col = Mysql.column result in
  let rec loop = function
    | None -> []
    | Some x -> parse_todo col x :: loop (Mysql.fetch result)
  in
  loop (Mysql.fetch result)

let read_todo id =
  let result = Mysql.(exec dbd (sprintf "select * from todo_app.todo where id = %d" id)) in
  let col = Mysql.column result in
  match Mysql.fetch result with
    | Some x -> Some (parse_todo col x)
    | None -> None

let string_of_todo (id, title, content, status, created_at, updated_at) = 
  sprintf "%d: [%s] %s [status: %d] %s/%s" id title content status created_at updated_at

let echo () =
  Server.respond_string ~status:`OK ~body:"echo" ()

let get_todos () =
  let todos = List.map string_of_todo (read_todos ()) |> String.concat "\n" in
  Server.respond_string ~status:`OK ~body:todos ()

let get_todo id =
  match read_todo id with
    | Some todo -> Server.respond_string ~status:`OK ~body:(string_of_todo todo) ()
    | None -> Server.respond_not_found ()

let create_todo body = Server.respond_string ~status:`Created ~body:"\n" ()

let update_todo body id = Server.respond_string ~status:`Created ~body:"\n" ()
  
let routes body =
  let open Routes in
  let open Code in
  function
    | `GET -> one_of 
      [ s "todos" /? nil @--> get_todos ()
      ; s "todos" / int /? nil @--> get_todo
      ; s "echo" /? nil @--> echo ()
      ]
    | `POST -> one_of
      [ s "todos" /? nil @--> create_todo body 
      ]
    | `PUT -> one_of
      [ s "todos" / int /? nil @--> update_todo body 
      ]
    | _ -> one_of []

let server =
  let callback conn req body =
    let target = Uri.path @@ Cohttp.Request.uri req in
    let meth = Request.meth req in
    match Routes.match' (routes body meth) ~target with
      | Some u -> u
      | None -> Server.respond_not_found ~uri:(Cohttp.Request.uri req) ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = Lwt_main.run server