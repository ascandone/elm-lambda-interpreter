module Lambda.Ast exposing (Lambda(..), toString)


type Lambda
    = Variable String
    | Abstraction String Lambda
    | Application Lambda Lambda


multiArgs : List String -> Lambda -> String
multiArgs bindings t =
    case t of
        Abstraction binding body ->
            multiArgs (binding :: bindings) body

        _ ->
            "λ " ++ (String.join " " <| List.reverse bindings) ++ "." ++ toString t


toString : Lambda -> String
toString ast =
    case ast of
        Abstraction binding body ->
            multiArgs [ binding ] body

        Application ((Application _ _) as f) x ->
            toString f ++ " " ++ parentesize x

        Application f x ->
            parentesize f ++ " " ++ parentesize x

        Variable id ->
            id


parentesize : Lambda -> String
parentesize t =
    case t of
        Variable _ ->
            toString t

        _ ->
            "(" ++ toString t ++ ")"
