module Lambda.Semantics exposing (ReductionType(..), Reductions, reduction, reductions)

import Lambda.Ast as Ast exposing (Lambda(..))


isFreeIn : String -> Lambda -> Bool
isFreeIn var term =
    case term of
        Variable var1 ->
            var == var1

        Application f x ->
            isFreeIn var f || isFreeIn var x

        Abstraction binding body ->
            binding /= var && isFreeIn var body


firstNotFree : String -> Lambda -> String
firstNotFree var term =
    if not (isFreeIn var term) then
        var

    else
        firstNotFree (var ++ "'") term


subst : String -> Lambda -> Lambda -> ( Lambda, Maybe ( Lambda, Lambda ) )
subst from to term =
    case term of
        Application f x ->
            case subst from to f of
                ( f_, (Just conversion) as c ) ->
                    ( Application f_ x, c )

                ( f_, Nothing ) ->
                    case subst from to x of
                        ( x_, b ) ->
                            ( Application f_ x_, b )

        Variable var1 ->
            if from == var1 then
                ( to, Nothing )

            else
                ( term, Nothing )

        Abstraction binding body ->
            if from == binding then
                ( term, Nothing )

            else
                let
                    newBinding =
                        firstNotFree binding to
                in
                if newBinding == binding then
                    Tuple.mapFirst (Abstraction binding) (subst from to body)

                else
                    case subst binding (Variable newBinding) body of
                        ( result, Nothing ) ->
                            ( Abstraction newBinding result, Just ( term, Abstraction newBinding result ) )

                        ( result, _ ) ->
                            -- This should never be executed (TODO: proof)
                            ( result, Nothing )


type ReductionType
    = Initial
    | BetaReduction Lambda Lambda
    | AlphaConversion Lambda Lambda


reduction : Lambda -> Maybe ( Lambda, ReductionType )
reduction term =
    case term of
        Application ((Abstraction binding body) as f) arg ->
            case subst binding arg body of
                ( newBody, Just ( from, to ) ) ->
                    Just ( Application (Abstraction binding newBody) arg, AlphaConversion from to )

                ( l, Nothing ) ->
                    Just ( l, BetaReduction f arg )

        Abstraction binding body ->
            reduction body
                |> Maybe.map (Tuple.mapFirst <| Abstraction binding)

        Application f x ->
            case reduction f of
                Just fReduction_ ->
                    Just <| Tuple.mapFirst (\r -> Application r x) fReduction_

                Nothing ->
                    Maybe.map (Tuple.mapFirst (Application f)) (reduction x)

        Variable _ ->
            Nothing


type alias Reductions =
    ( List ( Lambda, ReductionType ), Maybe Lambda )


{-| Returns a batch of "limit" reductions, and an optional continuation
-}
reductions : Int -> Lambda -> Reductions
reductions limit lambda =
    if limit <= 0 then
        ( [], Just lambda )

    else
        case reduction lambda of
            Nothing ->
                ( [], Nothing )

            Just (( ast, _ ) as reduction_) ->
                reductions (limit - 1) ast
                    |> Tuple.mapFirst ((::) reduction_)
