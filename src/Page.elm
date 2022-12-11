module Page exposing (..)

import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Task


formElement : String -> Html msg -> Html msg
formElement label control =
    H.div
        [ Attr.class "flex flex-col gap-2"
        ]
        [ H.span
            [ Attr.class "text-xs"
            ]
            [ H.text label ]
        , H.span [] [ control ]
        ]


pageLoaderDiv : Html msg
pageLoaderDiv =
    H.div
        [ Attr.class "h-screen flex items-center justify-center"
        ]
        [ H.text "Loading..."
        ]


renderIcon : { icon : FeatherIcons.Icon, size : Int } -> Html msg
renderIcon { icon, size } =
    icon
        |> FeatherIcons.withSize (toFloat size)
        |> FeatherIcons.withStrokeWidth 2
        |> FeatherIcons.toHtml []


type alias HeaderConfig msg =
    { text : String, icon : FeatherIcons.Icon, msg : msg }


renderHeader : HeaderConfig msg -> Html msg
renderHeader { text, icon, msg } =
    H.div
        [ Attr.id "app-page-header"
        ]
        [ H.div [] []
        , H.div [] [ H.text text ]
        , H.div
            [ Ev.onClick msg
            ]
            [ renderIcon { icon = icon, size = 16 }
            ]
        ]


renderBody : Html msg -> Html msg
renderBody content =
    H.div [ Attr.id "app-page-content" ]
        [ content ]


viewToaster : Maybe String -> Html msg
viewToaster string =
    case string of
        Nothing ->
            H.span [] []

        Just str ->
            H.div
                [ Attr.class "fixed bottom-5 left-5 right-5 bg-blue-700 text-white p-3 rounded text-xs"
                ]
                [ H.text str
                ]


type alias Token =
    String


type alias SheetId =
    String


type alias Accounts =
    List String


type alias Error =
    String


type alias Global =
    { token : String
    , sheetId : Maybe String
    , accounts : List String
    , sheetError : Error
    , accountSheet : Maybe String
    , expenseSheet : Maybe String
    , accountsLoading : Bool
    , authError : Maybe String
    , currentDate : String
    }


initGlobal =
    Global "" Nothing [] ""


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.perform (\_ -> msg) (Task.succeed 1)


errToString : Http.Error -> String
errToString e =
    case e of
        Http.BadUrl s ->
            s

        Http.BadStatus int ->
            "Server responded with a bad status code: " ++ String.fromInt int ++ ". This could be due to bad data (like wrong sheet name or incorrect values)."

        Http.BadBody s ->
            s

        Http.NetworkError ->
            "Network error! Unable to make a request."

        Http.Timeout ->
            "Network timeout! Took too long and there is no response yet."


accountsSheetDefault =
    "accounts"


expenseSheetDefault =
    "expense"


showError : String -> Html msg
showError error =
    H.div
        [ Attr.class "p-2 rounded bg-red-100 text-xs mb-5"
        ]
        [ H.text error ]
