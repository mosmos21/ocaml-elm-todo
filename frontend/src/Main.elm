module Main exposing (Model, Msg(..), main, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD
import Json.Encode as Encode


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type RequestStatus
    = LOADING
    | SUCCESS
    | FAILED


type TodoStatus
    = NOT_START
    | IN_PROGRESS
    | COMPLETE


type alias Todo =
    { id : Int
    , title : String
    , content : String
    , status : TodoStatus
    , created_at : String
    , updated_at : String
    }


type alias Model =
    { todos : List Todo
    , title : String
    , content : String
    , requestStatus : RequestStatus
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "" "" LOADING, getTodos )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = GotTodos (Result Http.Error (List Todo))
    | ChangeTitle String
    | ChangeContent String
    | Created (Result Http.Error ())
    | Updated (Result Http.Error ())
    | ClickSubmit
    | StartTodo Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTodos result ->
            case result of
                Ok todos ->
                    ( { model | todos = todos, requestStatus = SUCCESS }, Cmd.none )

                Err _ ->
                    ( { model | requestStatus = FAILED }, Cmd.none )

        ChangeTitle title ->
            ( { model | title = title }, Cmd.none )

        ChangeContent content ->
            ( { model | content = content }, Cmd.none )

        Created _ ->
            ( model, getTodos )

        Updated _ ->
            ( model, getTodos )

        ClickSubmit ->
            ( model, createTodo model.title model.content )

        StartTodo id status ->
            ( model, updateTodoStatus id status )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "TODO"
    , body =
        [ div
            [ style "width" "500px"
            , style "margin" "10px auto"
            ]
            [ h1 [] [ text "Todo List" ]
            , div
                [ style "border" "1px solid gray"
                , style "padding" "8px"
                ]
                [ textInput "title" "Title" ChangeTitle
                , textInput "content" "Content" ChangeContent
                , button
                    [ style "margin" "8px auto"
                    , style "width" "90%"
                    , onClick ClickSubmit
                    ]
                    [ text "Submit" ]
                ]
            , div [ style "margin-top" "16px" ] (List.map todoCard model.todos)
            ]
        ]
    }


textInput : String -> String -> (String -> msg) -> Html msg
textInput i lbl inputMsg =
    div []
        [ label [ for i ] [ text lbl ]
        , input
            [ id i
            , style "display" "block"
            , style "width" "90%"
            , style "margin" "0 auto"
            , onInput inputMsg
            ]
            []
        ]


todoCard : Todo -> Html Msg
todoCard todo =
    div
        [ style "margin" "16px 0"
        , style "border" "1px solid gray"
        , style "padding" "8px"
        ]
        [ h3 [] [ text (todo.title ++ " [" ++ statusToString todo.status ++ "]") ]
        , p [] [ text todo.content ]
        , div []
            [ span
                [ style "font-size" "12px"
                , style "color" "gray"
                ]
                [ text ("created_at: " ++ todo.created_at ++ " / updated_at: " ++ todo.updated_at)
                ]
            ]
        , case todo.status of
            NOT_START ->
                button [ onClick (StartTodo todo.id 1) ] [ text "Start" ]

            IN_PROGRESS ->
                button [ onClick (StartTodo todo.id 2) ] [ text "Complete" ]

            COMPLETE ->
                span [] []
        ]


statusToString : TodoStatus -> String
statusToString s =
    case s of
        NOT_START ->
            "NOT_START"

        IN_PROGRESS ->
            "IN_PROGRESS"

        COMPLETE ->
            "COMPLETE"



-- HTTP


getTodos : Cmd Msg
getTodos =
    Http.get
        { url = "/api/todos"
        , expect = Http.expectJson GotTodos todoListDecoder
        }


createTodo : String -> String -> Cmd Msg
createTodo title content =
    Http.post
        { url = "/api/todos"
        , body =
            jsonBody
                (Encode.object
                    [ ( "title", Encode.string title )
                    , ( "content", Encode.string content )
                    ]
                )
        , expect = Http.expectWhatever Created
        }


updateTodoStatus : Int -> Int -> Cmd Msg
updateTodoStatus id status =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Content-Type" "application/json" ]
        , url = "/api/todos/" ++ String.fromInt id
        , body = jsonBody (Encode.object [ ( "status", Encode.int status ) ])
        , expect = Http.expectWhatever Updated
        , timeout = Nothing
        , tracker = Nothing
        }


todoListDecoder : JD.Decoder (List Todo)
todoListDecoder =
    JD.list todoDecoder


todoDecoder : JD.Decoder Todo
todoDecoder =
    JD.map6 Todo
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "content" JD.string)
        (JD.field "status" JD.int
            |> JD.andThen
                (\i ->
                    case statusFromInt i of
                        Just a ->
                            JD.succeed a

                        Nothing ->
                            JD.fail "invalid todo status"
                )
        )
        (JD.field "created_at" JD.string)
        (JD.field "updated_at" JD.string)


statusFromInt : Int -> Maybe TodoStatus
statusFromInt i =
    case i of
        0 ->
            Just NOT_START

        1 ->
            Just IN_PROGRESS

        2 ->
            Just COMPLETE

        _ ->
            Nothing
