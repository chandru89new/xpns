module SettingsPage exposing (..)

import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Page


type Msg
    = UpdateSheetId String
    | GoToHomePage


pageHeader : Page.HeaderConfig Msg
pageHeader =
    { text = "Settings"
    , icon = FeatherIcons.home
    , msg = GoToHomePage
    }


pageWrapper : Html Msg -> Html Msg
pageWrapper body =
    H.div []
        [ Page.renderHeader pageHeader
        , Page.renderBody body
        ]


view : String -> Html Msg
view sheetId =
    pageWrapper <|
        H.div []
            [ Page.formElement "Sheet ID:" <|
                H.input
                    [ Attr.value sheetId
                    , Ev.onInput UpdateSheetId
                    ]
                    []
            ]
