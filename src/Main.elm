module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    Int


type Msg
    = NoOp
    | Increment
    | Decrement


init : Model
init =
    1


view : Model -> H.Html Msg
view model =
    H.div
        [ Attr.class "h-screen w-100 flex flex-col items-center justify-center gap-y-8"
        ]
        [ H.button [ Ev.onClick Increment ] [ H.text "+" ]
        , H.div [ Attr.class "text-4xl" ] [ H.text <| String.fromInt model ]
        , H.button [ Ev.onClick Decrement ] [ H.text "-" ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

        NoOp ->
            model
