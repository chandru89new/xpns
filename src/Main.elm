module Main exposing (..)

import Browser
import Html as H


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


init =
    1


view _ =
    H.div [] [ H.text "Hi" ]


update _ model =
    model
