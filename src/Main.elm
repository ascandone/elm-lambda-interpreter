module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict
import FeatherIcons
import Html as H exposing (Html, input)
import Html.Attributes as A exposing (class, classList)
import Html.Events as E
import Lambda.Ast as Ast exposing (Lambda)
import Lambda.Parser exposing (ParseError(..), ParseResult(..), parse)
import Lambda.Semantics exposing (ReductionType(..), Reductions, reductions)
import Set exposing (Set)
import Task



{-
      TODO:

   # Minor
       * preventDefault in key shortcut
       * optimizations
       * animation on "LoadMore"

   # Major
       * aliases
       * sync with url
       * change paths/semantics

    # Docs
        * parsing errors
        * grammar
        * aliases

-}


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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


init : Model
init =
    { prompt = ""
    , reductions = Ok (Reductions [] Nothing)
    , aliases = []
    , collapsed = Set.empty
    }


type Msg
    = Input String
    | LoadMore
    | ParsePrompt
    | ToggleCollapse Int
    | FocusPrompt
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            -- TODO: reductions = Nothing ?
            ( { model | prompt = s, reductions = init.reductions }, Cmd.none )

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
                    ( { model | aliases = ( name, value ) :: model.aliases, prompt = "" }
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

        Noop ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    H.div [ class "sm:text-lg max-w-6xl w-full mx-auto  px-2" ]
        [ H.map never viewHeader
        , H.div [] <|
            List.map viewDeclaration model.aliases
        , viewPrompt model.prompt
        , H.div [ class "m-4" ] []
        , case model.reductions of
            Err e ->
                H.div [ class "text-red-600 text-center p-4" ]
                    [ H.text <|
                        case e of
                            AliasError a ->
                                "Alias not found: " ++ a

                            SyntaxError _ ->
                                "Syntax error"
                    ]

            Ok { batch, continuation } ->
                let
                    viewReduction_ i r =
                        H.map ((|>) i) (viewReduction (not <| Set.member i model.collapsed) r)
                in
                H.div []
                    [ H.div [ class "space-y-2" ]
                        (batch
                            |> List.indexedMap viewReduction_
                        )
                    , when (continuation /= Nothing) <|
                        H.div [ class "my-5" ] [ loadBtn ]
                    ]
        ]


viewDeclaration : ( String, Lambda ) -> Html msg
viewDeclaration ( name, value ) =
    H.div
        [ class "text-gray-600 font-mono font-light" ]
        [ H.text "let "
        , H.text name
        , H.text " = "
        , H.text <| Ast.toString value
        ]


viewHeader : Html Never
viewHeader =
    H.div [ class "max-w-2xl text-base mx-auto" ]
        [ H.div [ class "mt-2" ] []
        , H.h1 [ class "text-4xl font-semibold" ] [ H.text "Lambda calculus interpreter" ]
        , H.div [ class "mt-2" ] []
        , H.p [ class "text-gray-700 font-light" ] [ H.text "Lorem ipsum dolor sit amet consectetur adipisicing elit. Sunt nesciunt sint dolore fugit quos repellat aspernatur veniam pariatur deleniti, molestiae temporibus eos dolorum ab officiis quia earum aperiam blanditiis provident." ]
        , H.div [ class "mt-8" ] []
        ]


viewReductionIcon : String -> Html msg
viewReductionIcon str =
    H.span
        [ class "px-2 sm:px-3 py-2 text-gray-500 font-light text-right" ]
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
                , class "w-full text-gray-800 flex-1 bg-transparent leading-none py-2 px-2 bg-gray-200 rounded"
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
            [ class "text-gray-800 bg-pink-100 rounded px-4 py-3 leading-none tracking-wide font-light "
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
