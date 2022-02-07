module HomePage exposing (..)

import Auth exposing (Msg)
import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Page


type alias Model =
    String


init : Model
init =
    "1"


type Msg
    = GoToExpensePage
    | GoToSettingsPage
    | GoToTransferPage
    | GoToIncomePage
    | GoToRecentsPage
    | Logout


renderOption : FeatherIcons.Icon -> String -> Msg -> Html Msg
renderOption icon text msg =
    H.div
        [ Ev.onClick msg
        , Attr.class "rounded p-5 border border-gray-200 flex items-center justify-between gap-5"
        ]
        [ H.div
            [ Attr.class "flex gap-5 items-center"
            ]
            [ H.span [] [ Page.renderIcon { icon = icon, size = 16 } ]
            , H.span [] [ H.text text ]
            ]
        , H.span [] [ Page.renderIcon { icon = FeatherIcons.chevronRight, size = 24 } ]
        ]


view : Html Msg
view =
    H.div []
        [ Page.renderHeader
            { icon = FeatherIcons.settings
            , text = "Dashboard"
            , msg = GoToSettingsPage
            }
        , Page.renderBody <|
            H.div
                [ Attr.class "flex flex-col gap-5"
                ]
                [ renderOption FeatherIcons.arrowUpCircle "Expense" GoToExpensePage
                , renderOption FeatherIcons.dollarSign "Income" GoToIncomePage
                , renderOption FeatherIcons.refreshCw "Transfer" GoToTransferPage
                , renderOption FeatherIcons.activity "Recent" GoToRecentsPage
                , renderOption FeatherIcons.logOut "Logout" Logout
                ]
        ]
