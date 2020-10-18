module Lambda.Parser exposing (ParseError(..), ParseResult(..), parse)

import Dict exposing (Dict)
import Html exposing (del)
import Lambda.Ast as Ast exposing (Lambda(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , chompWhile
        , deadEndsToString
        , oneOf
        , succeed
        , symbol
        , variable
        )
import Set


type RawLambda
    = RAbstraction String (List String) RawLambda
    | RApplication RawLambda (List RawLambda)
    | RVariable String
    | Alias String


unwrapResults : List (Result a b) -> Result a (List b)
unwrapResults =
    List.foldr (Result.map2 (::)) (Ok [])


desugarLambda : Dict String Lambda -> RawLambda -> Result String Lambda
desugarLambda aliases rawLambda =
    case rawLambda of
        RAbstraction x xs body ->
            desugarLambda aliases body
                |> Result.map (\body_ -> Abstraction x <| List.foldl Abstraction body_ xs)

        RApplication term terms ->
            let
                rterm =
                    desugarLambda aliases term

                rterms =
                    (unwrapResults << List.map (desugarLambda aliases)) terms
            in
            Result.map2 (List.foldr (\m n -> Application n m)) rterm rterms

        RVariable v ->
            Ok (Variable v)

        Alias s ->
            -- TODO TEMP
            case Dict.get s aliases of
                Just s_ ->
                    Ok s_

                Nothing ->
                    Err <| "Alias not found: " ++ s



{-

   Id := \x.x
   K := \y _.y
   E := Id (\x.x)

   Id K E
-}
-- evaluateDeclarations : List Declaration -> Result String (Dict String RawLambda)
-- evaluateDeclarations =
--     List.map (desugarLambda Dict.empty)
-- List.foldr
--     (\{ name, value } rest ->
--         Dict.insert name (desugarLambda rest value) rest
--     )
--     (Ok (Dict.fromList []))
-- case declarations of
--     [] ->
--         Dict.fromList []
--     { name, value } :: tl ->
--         Dict.union
--             (Dict.fromList [ ( name, value ) ])
--             (evaluateDeclarations tl)
-- desugarLambda : Dict String Lambda -> RawLambda -> Result String Lambda
-- desugarLambda dict rawLambda =
--     case declarations of
--         [] ->
--             desugarLambda dict rawLambda
--         { name, value } :: tl ->
--             case desugarLambda Dict.empty value of
--                 Ok lambda ->
--                     desugarLambda (Dict.insert name lambda dict)
--                         { declarations = tl
--                         , expression = expression
--                         }
--                 (Err _) as e ->
--                     e


variable : Parser RawLambda
variable =
    succeed RVariable
        |= identifier


alias_ : Parser RawLambda
alias_ =
    succeed Alias
        |= aliasIdentifier


applications : Parser RawLambda
applications =
    succeed RApplication
        |= applicable
        |. spaces
        |= sepBy spaces applicable


applicable : Parser RawLambda
applicable =
    oneOf
        [ variable
        , alias_
        , parenthesized <|
            Parser.lazy <|
                \_ ->
                    oneOf
                        [ applications
                        , abstraction
                        ]
        ]


abstraction : Parser RawLambda
abstraction =
    succeed RAbstraction
        |. oneOf [ symbol "\\", symbol "λ" ]
        |. spaces
        |= identifier
        |. spaces
        |= sepBy spaces identifier
        |. symbol "."
        |. spaces
        |= oneOf
            [ applications
            , Parser.lazy (\_ -> abstraction)
            ]


lambdaTerm : Parser RawLambda
lambdaTerm =
    succeed identity
        |. spaces
        |= oneOf [ abstraction, applications ]


declaration : Parser RawParseResult
declaration =
    succeed RawDeclaration
        |. Parser.keyword "let"
        |. spaces
        |= aliasIdentifier
        |. spaces
        |. symbol "="
        |. spaces
        |= lambdaTerm


topLevel : Parser RawParseResult
topLevel =
    oneOf
        [ declaration
        , Parser.map RawLambda lambdaTerm
        ]


type ParseError
    = SyntaxError (List DeadEnd)
    | AliasError String


type RawParseResult
    = RawLambda RawLambda
    | RawDeclaration String RawLambda


type ParseResult
    = Lambda Lambda
    | Declaration String Lambda


parse : Dict String Lambda -> String -> Result ParseError ParseResult
parse aliases src =
    case Parser.run topLevel src of
        Err e ->
            Err <| SyntaxError e

        Ok (RawLambda rawLambda) ->
            case desugarLambda aliases rawLambda of
                Err e ->
                    Err <| AliasError e

                Ok lambda ->
                    Ok <| Lambda lambda

        Ok (RawDeclaration name value) ->
            case desugarLambda aliases value of
                Err e ->
                    Err <| AliasError e

                Ok lambda ->
                    Ok <| Declaration name lambda



-- RawLambda _ ->
--     Debug.todo "rawLambda"
-- RawDeclaration name expr ->
--     Debug.todo "rawLambda"
-- Ok (Ok s) ->
--     Ok s
-- Ok (Err e) ->
--     Err (AliasError e)
-- Err deadEnds ->
--     Err (SyntaxError deadEnds)


reserved : Set.Set String
reserved =
    Set.fromList [ "let", "=", "\\", "." ]



-- Helpers


parenthesized : Parser a -> Parser a
parenthesized parser =
    succeed identity
        |. symbol "("
        |. spaces
        |= parser
        |. spaces
        |. symbol ")"


spaces : Parser ()
spaces =
    chompWhile ((==) ' ')


identifier : Parser String
identifier =
    Parser.variable
        { start = \ch -> Char.isLower ch || ch == '_'
        , inner = \ch -> Char.isLower ch || ch == '_' || ch == '\''
        , reserved = reserved
        }


aliasIdentifier : Parser String
aliasIdentifier =
    Parser.variable
        { start = Char.isUpper
        , inner = Char.isAlpha
        , reserved = reserved
        }


sepBy : Parser a -> Parser b -> Parser (List b)
sepBy separator term =
    let
        loop reversed =
            oneOf
                [ succeed (\t -> Loop (t :: reversed))
                    |= term
                    |. separator
                , succeed <| Done reversed
                ]
    in
    Parser.loop [] loop
