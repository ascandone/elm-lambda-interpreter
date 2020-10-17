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



{-

    (\x y . x y) (y y y)

   - x is bound in "\y.x y"

   subst "x" "y y y" "\y.x" =>
   \y'.y

   (\y.x y)[y y y/x]

   - y is bound in "y y y"

   =>
   (\y'.x y')[y y y/x]
   =>
   (\y'.x y')[y y y/x]



-}
{-
   if False is returned, an alpha-conversion was performed instead of the requested substitions
-}
--  (λx. k (\ y.x ) (\ y.x ) ) y


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
                            Debug.todo "this should crash"


type ReductionType
    = Initial
    | BetaReduction Lambda Lambda
    | AlphaConversion Lambda Lambda



-- TODO: implement different semantics


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



-- TODO: limit to n


type alias Reductions =
    { batch : List ( Lambda, ReductionType )
    , continuation : Maybe Lambda
    }


reductions : Int -> Lambda -> Reductions
reductions limit lambda =
    if limit <= 0 then
        Reductions [] (Just lambda)

    else
        case reduction lambda of
            Nothing ->
                Reductions [] Nothing

            Just (( ast, _ ) as reduction_) ->
                let
                    { batch, continuation } =
                        reductions (limit - 1) ast
                in
                Reductions (reduction_ :: batch) continuation



{-
   reduction : Lambda -> Lambda
   reduction term =
       case term of
           Application (Abstraction binding body) arg ->
               subst binding arg body
           Application f x ->
               Application (reduction f) (reduction x)
           Abstraction binding body ->
               Abstraction binding (reduction body)
           Variable _ ->
               term
-}
{-

   subst : String -> Lambda -> Lambda -> Lambda
   subst from to term =
       case term of
           Application f x ->
               Application (subst from to f) (subst from to x)

           Variable var1 ->
               if from == var1 then
                   to

               else
                   term

           Abstraction binding body ->
               if from == binding then
                   term

               else
                   let
                       newBinding =
                           firstNotFree binding to

                       rebind =
                           if binding == newBinding then
                               identity

                           else
                               subst binding (Variable newBinding)
                   in
                   Abstraction newBinding <|
                       (body
                           |> rebind
                           |> subst from to
                       )
-}
