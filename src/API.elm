module API exposing (..)

import Http exposing (Expect)


type alias Token =
    String


type alias BaseURL =
    String


type alias QueryParams =
    List ( String, String )


post : Token -> BaseURL -> QueryParams -> Http.Body -> Expect msg -> Cmd msg
post token baseURL queryParams body expect =
    let
        queryParamsAsString =
            queryParams
                |> List.map (\( f, s ) -> f ++ "=" ++ s)
                |> List.intersperse "&"
                |> String.concat

        constructedURL =
            baseURL ++ "?" ++ queryParamsAsString
    in
    Http.request
        { method = "POST"
        , url = constructedURL
        , body = body
        , headers =
            [ Http.header "Authorization" <| "Bearer " ++ token
            , Http.header "Content-type" "application/json"
            ]
        , timeout = Nothing
        , tracker = Nothing
        , expect = expect
        }


get : Token -> BaseURL -> QueryParams -> Expect msg -> Cmd msg
get token baseURL queryParams expect =
    let
        queryParamsAsString =
            queryParams
                |> List.map (\( f, s ) -> f ++ "=" ++ s)
                |> List.intersperse "&"
                |> String.concat

        constructedURL =
            baseURL ++ "?" ++ queryParamsAsString
    in
    Http.request
        { method = "GET"
        , url = constructedURL
        , body = Http.emptyBody
        , headers =
            [ Http.header "Authorization" <| "Bearer " ++ token
            , Http.header "Content-type" "application/json"
            ]
        , timeout = Nothing
        , tracker = Nothing
        , expect = expect
        }
