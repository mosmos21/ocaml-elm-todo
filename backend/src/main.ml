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
      | x -> row_parser col x :: loop (Mysql.fetch result)
    in
    loop (Mysql.fetch result)
end

let dbd = DbUtil.connect_with_tcp ()

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
end

module TodoRepository = struct
  let parse_todo col = function
    | None -> None
    | Some x ->
      let open Mysql in
      Some Todo.(
        { id         = not_null int2ml (col ~key:"id" ~row: x)
        ; title      = not_null str2ml (col ~key:"title" ~row: x)
        ; content    = not_null str2ml (col ~key:"content" ~row: x)
        ; status     = status_of_int @@ not_null int2ml (col ~key:"status" ~row: x)
        ; created_at = not_null str2ml (col ~key:"created_at" ~row: x)
        ; updated_at =  not_null str2ml (col ~key:"updated_at" ~row: x)
        }
      )

  let select_todos () =
    let result = Mysql.(exec dbd "select * from todo_app.todo") in
    DbUtil.parse_rows parse_todo result

  let select_todo id =
    let result = Mysql.exec dbd (sprintf "select * from todo_app.todo where id = %d" id) in
    let col = Mysql.column result in
    parse_todo col (Mysql.fetch result)
  
  let insert_todo title content =
    ignore(Mysql.exec dbd (sprintf "insert into todo_app(title, content) values (%s, %s)" title content))
end

module EchoHandler = struct
  let echo () =
    Server.respond_string ~status:`OK ~body:"echo" ()
end

module TodoHandler = struct
  let string_of_todo = function
    | None -> ""
    | Some (todo: Todo.t) -> Printf.sprintf "[id: %d] %s - %s" todo.id todo.title todo.content

  let get_todos () =
    let todos = List.map string_of_todo (TodoRepository.select_todos ()) |> String.concat "\n" in
    Server.respond_string ~status:`OK ~body:todos ()

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
      Server.respond_string ~status:`Created ~body:"" ()

  let update_todo body id = Server.respond_string ~status:`Created ~body:"\n" ()
end
  
(* ******** ******** ******** ******** *)

let routes body =
  let open Routes in
  let open Code in
  function
    | `GET -> one_of 
      [ s "todos" /? nil @--> TodoHandler.get_todos ()
      ; s "todos" / int /? nil @--> TodoHandler.get_todo
      ; s "echo" /? nil @--> EchoHandler.echo ()
      ]
    | `POST -> one_of
      [ s "todos" /? nil @--> TodoHandler.create_todo body 
      ]
    | `PUT -> one_of
      [ s "todos" / int /? nil @--> TodoHandler.update_todo body 
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