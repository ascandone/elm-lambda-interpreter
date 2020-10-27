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
    | Num Int


unwrapResults : List (Result a b) -> Result a (List b)
unwrapResults =
    List.foldr (Result.map2 (::)) (Ok [])


desugarLambda : Dict String Lambda -> RawLambda -> Result String Lambda
desugarLambda aliases rawLambda =
    case rawLambda of
        Num n ->
            let
                ( f, x ) =
                    ( "f", "x" )

                helper n_ acc =
                    if n_ <= 0 then
                        acc

                    else
                        helper (n_ - 1) (Application (Variable f) acc)
            in
            Ok <| Abstraction f <| Abstraction x <| helper n (Variable x)

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
                    Err <| s


variable : Parser RawLambda
variable =
    succeed RVariable
        |= identifier


num : Parser RawLambda
num =
    succeed Num
        |= Parser.int


alias_ : Parser RawLambda
alias_ =
    succeed Alias
        |= aliasIdentifier


applications : Parser RawLambda
applications =
    succeed (\( x, xs ) -> RApplication x xs)
        |= sepBy1 spaces applicable


applicable : Parser RawLambda
applicable =
    oneOf
        [ variable
        , num
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
    succeed (\( x, xs ) -> RAbstraction x xs)
        |. oneOf [ symbol "\\", symbol "λ" ]
        |. spaces
        |= sepBy1 spaces identifier
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
    succeed identity
        |= oneOf
            [ declaration
            , Parser.map RawLambda lambdaTerm
            ]
        |. Parser.end


type ParseError
    = SyntaxError (List DeadEnd)
    | AliasError String


type RawParseResult
    = RawLambda RawLambda
    | RawDeclaration String RawLambda


type ParseResult
    = Lambda Lambda
    | Declaration String Lambda


defaultAliases : List ( String, Lambda )
defaultAliases =
    [ ( "True", Abstraction "x" <| Abstraction "_" <| Variable "x" )
    , ( "False", Abstraction "_" <| Abstraction "x" <| Variable "x" )
    ]


parse : Dict String Lambda -> String -> Result ParseError ParseResult
parse aliases_ src =
    let
        aliases =
            List.foldr (\( n, t ) -> Dict.insert n t) aliases_ defaultAliases
    in
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



-- sepBy1 : Parser a -> Parser b -> Parser (List b)


sepBy1 : Parser ignore -> Parser a -> Parser ( a, List a )
sepBy1 separator term =
    let
        loop reversed =
            oneOf
                [ succeed (\t -> Loop (t :: reversed))
                    |= term
                    |. separator
                , succeed <| Done reversed
                ]
    in
    succeed (\x xs -> ( x, xs ))
        |= term
        |. separator
        |= Parser.loop [] loop
