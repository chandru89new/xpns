port module Capacitor exposing (..)

import Json.Encode


port showAlert : { title : String, message : String } -> Cmd msg


port saveToStorage : ( String, Json.Encode.Value ) -> Cmd msg
