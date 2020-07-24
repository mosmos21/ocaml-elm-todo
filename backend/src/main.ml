open Printf
open Lwt
open Cohttp
open Cohttp_lwt_unix

module DbUtil = struct
  let connect_with_tcp () = 
    Mysql.connect 
      ~options:[Mysql.OPT_PROTOCOL Mysql.PROTOCOL_TCP] 
      { dbhost   = Some "localhost"
      ;	dbname   = Some "todo_app"
      ; dbport   = Some 3306
      ; dbpwd    = Some "password"
      ; dbuser   = Some "todo_user"
      ;	dbsocket = None
      }

  let parse_rows row_parser result =
    let col = Mysql.column result in
    let rec loop = function
      | None -> []
      | Some x -> row_parser col x :: loop (Mysql.fetch result)
    in
    loop (Mysql.fetch result) 

  let dbd = connect_with_tcp ()
  
  let exec query = 
    Logs.info (fun m -> m "[EXEC QUERY] %s" query);
    Mysql.exec dbd query
end

module Todo = struct
  module Status = struct
    type t =
      | NOT_START
      | IN_PROGRESS
      | COMPLETE
  end

  type t = 
    { id : int
    ; title : string
    ; content : string
    ; status : Status.t
    ; created_at : string
    ; updated_at : string
    }

  let status_of_int = function
    | 0 -> Status.NOT_START
    | 1 -> Status.IN_PROGRESS
    | 2 -> Status.COMPLETE
    | _ -> Status.NOT_START

  let int_of_status = function
    | Status.NOT_START -> 0
    | Status.IN_PROGRESS -> 1
    | Status.COMPLETE -> 2

  let json_of_todo todo =
    Yojson.Basic.(
      `Assoc 
        [ ("id", `Int todo.id)
        ; ("title", `String todo.title)
        ; ("content", `String todo.content)
        ; ("status", `Int (int_of_status todo.status))
        ; ("created_at", `String todo.created_at)
        ; ("updated_at", `String todo.updated_at)
        ]
    )
  
  let json_of_todos todos =
    Yojson.Basic.(`List (List.map json_of_todo todos))
end

module TodoRepository = struct
  let parse_todo col x =
    let open Mysql in
    Todo.(
      { id         = not_null int2ml (col ~key:"id" ~row: x)
      ; title      = not_null str2ml (col ~key:"title" ~row: x)
      ; content    = not_null str2ml (col ~key:"content" ~row: x)
      ; status     = status_of_int @@ not_null int2ml (col ~key:"status" ~row: x)
      ; created_at = not_null str2ml (col ~key:"created_at" ~row: x)
      ; updated_at =  not_null str2ml (col ~key:"updated_at" ~row: x)
      }
    )

  let select_todos () =
    let result = DbUtil.exec "select * from todo order by id desc" in
    DbUtil.parse_rows parse_todo result 

  let select_todo id =
    let result = DbUtil.exec (sprintf "select * from todo where id = %d" id) in
    match Mysql.fetch result with
      | Some x -> let col = Mysql.column result in Some (parse_todo col x)
      | None -> None
  
  let insert_todo title content =
    ignore(DbUtil.exec (sprintf "insert into todo(title, content) values (\"%s\", \"%s\")" title content))

  let update_todo_status id status =
    ignore(DbUtil.exec (sprintf "update todo set status = %d, updated_at = now() where id = %d" status id))
end

module EchoHandler = struct
  let echo param =
    let body = sprintf "{ \"value\": \"%s\" }" param in
    Server.respond_string ~status:`OK ~body ()
end

module TodoHandler = struct
  let string_of_todo = function
    | None -> ""
    | Some (todo: Todo.t) -> Printf.sprintf "[id: %d] %s - %s" todo.id todo.title todo.content

  let get_todos () =
    let todos = TodoRepository.select_todos () |> Todo.json_of_todos in
    let body = Yojson.Basic.pretty_to_string todos in
    Server.respond_string ~status:`OK ~body ()

  let get_todo id =
    match TodoRepository.select_todo id with
      | None -> Server.respond_not_found ()
      | todo -> Server.respond_string ~status:`OK ~body:(string_of_todo todo) ()

  let create_todo body = Cohttp_lwt.Body.to_string body 
    >|= begin fun body ->
    let json_body = Yojson.Basic.from_string body in
      let open Yojson.Basic.Util in
      let title = json_body |> member "title" |> to_string in
      let content = json_body |> member "content" |> to_string in
      TodoRepository.insert_todo title content
    end
    >>= fun () -> 
      Server.respond ~status:`Created ~body:`Empty ()

  let update_todo body id = Cohttp_lwt.Body.to_string body
  >|= begin fun body ->
    let json_body = Yojson.Basic.from_string body in
    let status = Yojson.Basic.Util.(json_body |> member "status" |> to_int) in
    TodoRepository.update_todo_status id status
  end
  >>= fun () ->
    Server.respond ~status:`No_content ~body:`Empty ()
end
  
(* ******** ******** ******** ******** *)

let routes body =
  let open Routes in
  let open Code in
  function
    | `GET -> one_of 
      [ s "api" / s "todos" /? nil @--> TodoHandler.get_todos ()
      ; s "api" / s "todos" / int /? nil @--> TodoHandler.get_todo
      ; s "api" / s "echo" / str /? nil @--> EchoHandler.echo 
      ]
    | `POST -> one_of
      [ s "api" / s "todos" /? nil @--> TodoHandler.create_todo body 
      ]
    | `PUT -> one_of
      [ s "api" / s "todos" / int /? nil @--> TodoHandler.update_todo body 
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

let () = 
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run server