module Lambda.Parser exposing (ParseError(..), parse)

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


type alias Declaration =
    { name : String
    , value : RawLambda
    }


type alias Program =
    { declarations : List Declaration
    , expression : RawLambda
    }


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


desugarProgram : Dict String Lambda -> Program -> Result String Lambda
desugarProgram dict { declarations, expression } =
    case declarations of
        [] ->
            desugarLambda dict expression

        { name, value } :: tl ->
            case desugarLambda Dict.empty value of
                Ok lambda ->
                    desugarProgram (Dict.insert name lambda dict)
                        { declarations = tl
                        , expression = expression
                        }

                (Err _) as e ->
                    e


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


topLevel : Parser RawLambda
topLevel =
    succeed identity
        |. spaces
        |= oneOf [ abstraction, applications ]


declaration : Parser Declaration
declaration =
    succeed Declaration
        |. Parser.keyword "let"
        |. spaces
        |= aliasIdentifier
        |. spaces
        |. symbol "="
        |. spaces
        |= topLevel


program : Parser Program
program =
    succeed Program
        |= sepBy (symbol "\n") declaration
        |. chompWhile ((==) '\n')
        |= topLevel
        |. Parser.end


type ParseError
    = SyntaxError (List DeadEnd)
    | AliasError String


parse : String -> Result ParseError Lambda
parse src =
    case Result.map (desugarProgram Dict.empty) <| Parser.run program src of
        Ok (Ok s) ->
            Ok s

        Ok (Err e) ->
            Err (AliasError e)

        Err deadEnds ->
            Err (SyntaxError deadEnds)


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
