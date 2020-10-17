module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import FeatherIcons
import Html as H exposing (Html, input)
import Html.Attributes as A exposing (class, classList)
import Html.Events as E
import Lambda.Ast as Ast exposing (Lambda)
import Lambda.Parser exposing (ParseError(..), parse)
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
    , collapsed : Set Int
    }


init : Model
init =
    { prompt = """(λx y.x y) (y y y)"""
    , reductions = Ok (Reductions [] Nothing)
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
            ( { model | prompt = s }, Cmd.none )

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
            ( Tuple.first <|
                update LoadMore
                    { model
                        | reductions =
                            Result.map
                                (\t -> Reductions [ ( t, Initial ) ] (Just t))
                                (parse model.prompt)
                        , collapsed = Set.empty
                    }
            , Task.attempt (\_ -> Noop) (Browser.Dom.blur promptId)
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
        , viewPrompt model.prompt
        , H.div [ class "m-4" ] []
        , case model.reductions of
            Err _ ->
                H.div [ class "text-red-600 text-center p-4" ] [ H.text "Parsing error" ]

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


viewDeclaration : Html msg
viewDeclaration =
    H.span [ class "text-gray-600 font-mono font-light" ] [ H.text "let K = \\x y.x" ]


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
                        "."
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



-- [ H.text "Beta reduction of \\x.x and (x y z)" ]


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



{-




   view_ : Model -> Html Msg
   view_ model =
       H.div [ class "flex flex-col text-lg text-gray-900 md:flex-row md:full" ]
           [ -- Left side
             H.div [ class "flex-1 self-stretch px-4 py-8 h-64" ]
               [ H.textarea
                   [ class "h-full -my-1 focus:bg-gray-100  focus:outline-none w-full bg-transparent font-mono resize-none"
                   , A.autofocus True
                   , A.value model.input
                   , E.onInput Input
                   ]
                   []
               ]
           , H.div [ class "my-4 md:my-0 md:mx-4" ] []
           , -- Right side
             H.div [ class "md:h-screen flex-1 overflow-auto px-2 py-2" ] <|
               case parse model.input of
                   Ok ast ->
                       let
                           ( reductions, loadMore ) =
                               getReductions model.batchesLimit ast
                       in
                       [ H.div [] <| List.map viewStep (( ast, Initial ) :: reductions)
                       , if loadMore then
                           H.span [ class "pb-8" ]
                               [ H.button
                                   [ class "bg-indigo-100 px-2 py-1 rounded"
                                   , E.onClick LoadMore
                                   ]
                                   [ H.text "Load more" ]
                               ]

                         else
                           H.text ""
                       ]

                   Err e ->
                       [ H.span [ class "text-red-600 text-md" ]
                           [ H.text <|
                               case e of
                                   SyntaxError deadLines ->
                                       "Parsing error."

                                   AliasError e1 ->
                                       "Parsing error."
                           ]
                       ]
           ]

-}
{-

   getReductions : Int -> Lambda -> ( List ( Lambda, ReductionType ), Bool )
   getReductions batchesLimit lambda =
       let
           { batch, continuation } =
               reductions batchSize lambda
       in
       case ( batchesLimit, continuation ) of
           ( 1, Just _ ) ->
               ( batch, True )

           ( 1, Nothing ) ->
               ( batch, False )

           ( _, Just lambda1 ) ->
               Tuple.mapFirst (List.append batch) (getReductions (batchesLimit - 1) lambda1)

           ( _, Nothing ) ->
               ( batch, False )
-}
