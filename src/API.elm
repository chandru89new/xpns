module API exposing (..)

import Http exposing (Expect)
import Json.Decode as JsonD


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


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus String
    | BadBody String


expectJson : (Result Error a -> msg) -> JsonD.Decoder a -> Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ _ body ->
                    Err (BadStatus body)

                Http.GoodStatus_ _ body ->
                    case JsonD.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (JsonD.errorToString err))


errorToString : Error -> String
errorToString e =
    case e of
        BadUrl s ->
            s

        BadStatus body ->
            "Server responded with a bad status: " ++ body ++ ". That is all I know at this time."

        BadBody s ->
            s

        NetworkError ->
            "Network error! Unable to make a request."

        Timeout ->
            "Network timeout! Took too long and there is no response yet."
