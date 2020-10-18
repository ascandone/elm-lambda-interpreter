module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Dict
import FeatherIcons
import Html as H exposing (Html, input)
import Html.Attributes as A exposing (class, classList)
import Html.Events as E
import Lambda.Ast as Ast exposing (Lambda)
import Lambda.Parser exposing (ParseError(..), ParseResult(..), parse)
import Lambda.Semantics exposing (ReductionType(..), Reductions, reductions)
import Parser
import Set exposing (Set)
import Task
import Url exposing (Url)



{-
      TODO:

   # Minor
       * preventDefault in key shortcut
       * optimizations
       * animation on "LoadMore"

   # Major
       * sync with url
       * change paths/semantics

    # Docs
        * finish grammar

-}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type alias Flags =
    ()


onSlashPress : Sub Msg
onSlashPress =
    Browser.Events.onKeyPress E.keyCode
        |> Sub.map
            (\i ->
                case i of
                    47 ->
                        FocusPrompt

                    _ ->
                        Noop
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onSlashPress


batchSize : number
batchSize =
    10


promptId : String
promptId =
    "prompt"


type alias Model =
    { prompt : String
    , reductions : Result ParseError Reductions
    , aliases : List ( String, Lambda )
    , collapsed : Set Int
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { prompt = ""
      , reductions = Ok (Reductions [] Nothing)
      , aliases = []
      , collapsed = Set.empty
      }
    , Cmd.none
    )


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    Noop


onUrlChange : Url -> Msg
onUrlChange _ =
    Noop


type Msg
    = Input String
    | LoadMore
    | ParsePrompt
    | ToggleCollapse Int
    | FocusPrompt
    | Noop
    | DeleteAlias String
    | UpdateAlias String
    | ClickedExample String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            -- TODO: reductions = Nothing ?
            ( { model | prompt = s, reductions = Ok (Reductions [] Nothing) }, Cmd.none )

        LoadMore ->
            ( case model.reductions of
                Ok { batch, continuation } ->
                    case continuation of
                        Just continuation_ ->
                            let
                                reductions_ =
                                    reductions batchSize continuation_
                            in
                            { model | reductions = Ok <| Reductions (batch ++ reductions_.batch) reductions_.continuation }

                        _ ->
                            model

                _ ->
                    model
            , Cmd.none
            )

        ParsePrompt ->
            case parse (Dict.fromList model.aliases) model.prompt of
                Ok (Lambda lambda) ->
                    update LoadMore
                        { model
                            | reductions = Ok <| Reductions [ ( lambda, Initial ) ] (Just lambda)
                            , collapsed = Set.empty
                        }

                Ok (Declaration name value) ->
                    ( { model
                        | aliases = ( name, value ) :: (.aliases <| Tuple.first <| update (DeleteAlias name) model)
                        , prompt = ""
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { model | reductions = Err e }
                    , Cmd.none
                    )

        FocusPrompt ->
            ( model
            , Task.attempt (\_ -> Noop) (Browser.Dom.focus promptId)
            )

        ToggleCollapse 0 ->
            ( model, Cmd.none )

        ToggleCollapse i ->
            ( { model
                | collapsed =
                    if Set.member i model.collapsed then
                        Set.remove i model.collapsed

                    else
                        Set.insert i model.collapsed
              }
            , Cmd.none
            )

        DeleteAlias name ->
            ( { model | aliases = List.filter (\( n, _ ) -> n /= name) model.aliases }
            , Cmd.none
            )

        UpdateAlias name ->
            case List.head (List.filter (\( n, _ ) -> n == name) model.aliases) of
                Just ( _, value ) ->
                    { model | prompt = "let " ++ name ++ " = " ++ Ast.toString value }
                        |> update (DeleteAlias name)
                        |> Tuple.first
                        |> update FocusPrompt

                Nothing ->
                    ( model, Cmd.none )

        ClickedExample value ->
            model
                |> update (Input value)
                |> Tuple.first
                |> update ParsePrompt

        Noop ->
            ( model, Cmd.none )



-- View


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.ExpectingKeyword k ->
            "The keyword: " ++ "\"" ++ k ++ "\""

        Parser.ExpectingSymbol s ->
            "The symbol: " ++ "\"" ++ s ++ "\""

        Parser.ExpectingVariable ->
            "A variable"

        Parser.ExpectingInt ->
            "An integer"

        _ ->
            --  Debug.toString p
            "Something else"


viewError : String -> ParseError -> Html a
viewError str e =
    let
        ( header, body ) =
            case e of
                AliasError a ->
                    ( "Alias not found"
                    , [ H.p []
                            [ H.text <| "The alias \""
                            , H.span [ class "font-bold" ] [ H.text a ]
                            , H.text "\" was not found."
                            ]
                      ]
                    )

                SyntaxError deadLines ->
                    let
                        viewDeadline : Parser.DeadEnd -> List (Html msg)
                        viewDeadline { col, problem } =
                            [ H.text <| problemToString problem ++ " at: \""
                            , H.pre [ class "inline" ]
                                [ H.span [ class "font-light" ] [ H.text (String.slice 0 (col - 1) str) ]
                                , H.span [ class "underline wavy font-bold" ] [ H.text (String.slice (col - 1) 9999 str) ]
                                ]
                            , H.text "\""
                            ]
                    in
                    ( "Syntax error"
                    , [ H.p [] [ H.text <| "There was a syntax error." ]
                      , H.p [] [ H.text <| "I was expecting one of these:" ]
                      , H.ul [] (deadLines |> List.map (viewDeadline >> H.li [ class "list-disc list-inside" ]))
                      ]
                    )
    in
    H.div [ class "flex bg-red-100 rounded-md px-3 py-3 m-1" ]
        [ H.div [ class "self-start rounded-full my-1 p-1 bg-red-400 text-red-100" ]
            [ FeatherIcons.x
                |> FeatherIcons.withClass "h-3 w-3"
                |> FeatherIcons.toHtml []
            ]
        , H.div [ class "mr-3" ] []
        , H.div [ class "flex flex-col text-red-600" ]
            [ H.h1 [ class "font-semibold" ] [ H.text header ]
            , H.div [ class "mt-1" ] []
            , H.div [ class "font-light text-sm" ] body
            ]
        ]



-- ε
-- TODO: complete


grammar : String
grammar =
    """// Input of prompt
<input>
    := <assignment>
    | <lambda-expr>

// You can perform let bindings
<assignment>
    := let <alias-identifier> = <lambda-expr>

<lambda-expr>
    := <abstraction>
    | <applications>

<applications>
    := <identifier>


// You can also use "\\" instead of "λ"
<abstraction>
    := λ <abstraction-bindings>

<abstraction-bindings>
    := <identifier> <abstraction-bindings>
    | <identifier> . <lambda-expr>
"""


viewHelp : Html Msg
viewHelp =
    H.div [ class "max-w-4xl mx-auto px-4" ]
        [ H.div [ class "mt-8" ] []
        , H.h3 [ class "text-gray-600 text-base font-semibold" ] [ H.text "Examples" ]
        , H.div [ class "mt-2" ] []
        , let
            viewExample name value =
                H.li []
                    [ H.text name
                    , H.text ": "
                    , H.code
                        [ class "underline text-blue-500 font-bold cursor-pointer", E.onClick <| ClickedExample value ]
                        [ H.text value ]
                    ]
          in
          H.ul [ class "text-gray-600 list-disc space-y-1" ]
            [ viewExample "Identity" "\\x.x"
            , viewExample "Non-terminating" "(\\x.x x) (\\x. x x)"
            , viewExample "Let statement" "let K = \\x y. y"
            , viewExample "Using let value" "K a b"
            ]
        , H.div [ class "mt-8" ] []
        , H.h3 [ class "text-gray-600 text-base font-semibold" ] [ H.text "Grammar: (TODO: complete)" ]
        , H.div [ class "mt-2" ] []
        , H.pre
            [ class "text-gray-600 font-light overflow-x-auto" ]
            [ H.text grammar ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Lambda calculus interpreter"
    , body =
        [ H.div [ class "max-w-6xl w-full mx-auto px-2 py-6" ]
            [ when (not <| List.isEmpty model.aliases) <|
                H.div [ class "space-y-4 pb-4" ]
                    (model.aliases
                        |> List.map viewDeclaration
                        |> List.reverse
                    )
            , viewPrompt model.prompt
            , H.div [ class "m-4" ] []
            , case model.reductions of
                Err e ->
                    viewError model.prompt e

                Ok { batch, continuation } ->
                    case batch of
                        [] ->
                            viewHelp

                        _ ->
                            let
                                viewReduction_ i r =
                                    H.map ((|>) i) (viewReduction (not <| Set.member i model.collapsed) r)
                            in
                            H.div []
                                [ H.div [ class "space-y-2" ]
                                    (batch |> List.indexedMap viewReduction_)
                                , when (continuation /= Nothing) <|
                                    H.div [ class "my-5" ] [ loadBtn ]
                                ]
            ]
        ]
    }


viewDeclaration : ( String, Lambda ) -> Html Msg
viewDeclaration ( name, value ) =
    H.div [ class "flex items-center group", E.onDoubleClick <| UpdateAlias name ]
        [ H.div
            [ class "flex-1 xl:flex-none ml-2 text-gray-400 font-mono font-light" ]
            [ H.text "let "
            , H.span [ class "text-gray-800 font-bold" ] [ H.text name ]
            , H.text " = "
            , H.span [ class "text-gray-800" ] [ H.text <| Ast.toString value ]
            ]
        , H.div [ class "xl:mr-8" ] []
        , FeatherIcons.edit2
            |> FeatherIcons.withClass "h-5 text-gray-700 md:text-gray-400 group-hover:text-gray-700 cursor-pointer  "
            |> FeatherIcons.toHtml [ E.onClick <| UpdateAlias name ]
        , H.div [ class "mr-2" ] []
        , FeatherIcons.trash
            |> FeatherIcons.withClass "h-5 text-red-600 md:text-red-300 group-hover:text-red-600 cursor-pointer  group-hover:block"
            |> FeatherIcons.toHtml [ E.onClick <| DeleteAlias name ]
        ]


viewHeader : Html Never
viewHeader =
    H.div [ class "max-w-2xl text-base mx-auto px-2" ]
        [ H.div [ class "mt-2" ] []
        , H.h1 [ class "text-4xl font-semibold" ] [ H.text "Lambda calculus interpreter" ]
        , H.div [ class "mt-2" ] []
        , H.p [ class "text-gray-700 font-light" ] [ H.text "Lorem ipsum dolor sit amet consectetur adipisicing elit. Sunt nesciunt sint dolore fugit quos repellat aspernatur veniam pariatur deleniti, molestiae temporibus eos dolorum ab officiis quia earum aperiam blanditiis provident." ]
        , H.div [ class "mt-8" ] []
        ]


viewReductionIcon : String -> Html msg
viewReductionIcon str =
    H.span
        [ class "px-2 sm:px-3 py-2 text-gray-400 font-light text-right" ]
        [ H.text str ]


lineContainer : List (Html msg) -> Html msg
lineContainer =
    H.span [ class "flex items-center font-mono w-full" ]


viewPrompt : String -> Html Msg
viewPrompt value =
    lineContainer
        [ viewReductionIcon ">"
        , H.form
            [ class "flex-1"
            , E.onSubmit ParsePrompt
            ]
            [ H.input
                [ A.value value
                , A.id promptId
                , E.onInput Input
                , class "w-full text-gray-800 flex-1 bg-transparent leading-none py-2 px-2 bg-cool-gray-100 rounded"
                , class "focus:outline-none focus:shadow-outline focus:border-blue-300"
                , A.placeholder "Enter lambda term. (\"/\" to focus)"
                , A.autofocus True
                , A.type_ "text"
                , A.attribute "autocapitalize" "none"
                ]
                []
            ]
        ]


viewReduction : Bool -> ( Lambda, ReductionType ) -> Html (Int -> Msg)
viewReduction collapsed ( l, t ) =
    H.div [ class "border rounded cursor-pointer", A.classList [ ( "border-transparent", collapsed ) ], E.onClick ToggleCollapse ]
        [ lineContainer
            [ viewReductionIcon <|
                case t of
                    AlphaConversion _ _ ->
                        "α"

                    BetaReduction _ _ ->
                        "β"

                    Initial ->
                        "λ"
            , H.span
                [ class "text-gray-800 flex-1 leading-none" ]
                [ H.text <| Ast.toString l ]
            , when (t /= Initial) <|
                H.div
                    [ class "transition duration-200 transition-transform transform "
                    , A.classList [ ( "rotate-180", collapsed ) ]
                    ]
                    [ FeatherIcons.chevronDown
                        |> FeatherIcons.withClass
                            "h-5 box-content px-2 text-gray-800 cursor-pointer"
                        |> FeatherIcons.toHtml []
                    ]
            ]
        , when (t /= Initial && not collapsed) <|
            H.div [ class "px-2 pb-2 text-gray-700 font-light text-base" ] <|
                let
                    term t_ =
                        H.span [ class "font-semibold font-mono text-gray-800" ] [ H.text <| Ast.toString t_ ]
                in
                case t of
                    AlphaConversion a b ->
                        [ H.text "α-conversion from \""
                        , term a
                        , H.text "\" to  \""
                        , term b
                        , H.text "\""
                        ]

                    BetaReduction f x ->
                        [ H.text "β-reduction of \""
                        , term f
                        , H.text "\" and  \""
                        , term x
                        , H.text "\""
                        ]

                    _ ->
                        []
        ]


loadBtn : Html Msg
loadBtn =
    H.div [ class "flex justify-center w-full" ]
        [ H.button
            [ class "text-gray-800 bg-indigo-100 rounded shadow px-4 py-3 leading-none tracking-wide"
            , E.onClick LoadMore
            ]
            [ H.text "Load more" ]
        ]



-- Helpers


when : Bool -> Html msg -> Html msg
when b h =
    if b then
        h

    else
        H.text ""
