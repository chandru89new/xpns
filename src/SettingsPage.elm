module SettingsPage exposing (..)

import Capacitor
import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Encode as JsonE
import Page
import Ports


type Msg
    = UpdateSheetId String
    | UpdateAccountSheetName String
    | UpdateExpenseSheetName String
    | SaveSheetSettings
    | GoToHomePage
    | Reset


type alias Model =
    { sheetId : String
    , accountSheet : String
    , expenseSheet : String
    }


init =
    Model "" "" ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSheetId id ->
            ( { model | sheetId = id }, Cmd.none )

        UpdateAccountSheetName name ->
            ( { model | accountSheet = name }, Cmd.none )

        UpdateExpenseSheetName name ->
            ( { model | expenseSheet = name }, Cmd.none )

        GoToHomePage ->
            ( model, Cmd.none )

        SaveSheetSettings ->
            ( model, saveSheetSettings model )

        Reset ->
            ( model, Ports.getSheetSettingsFromStorage () )


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


view : Model -> Html Msg
view { sheetId, accountSheet, expenseSheet } =
    pageWrapper <|
        H.div
            [ Attr.class "flex flex-col gap-10"
            ]
            [ Page.formElement "Sheet ID:" <|
                H.input
                    [ Attr.value sheetId
                    , Ev.onInput UpdateSheetId
                    ]
                    []
            , Page.formElement "Accounts sheet name:" <|
                H.input
                    [ Attr.value accountSheet
                    , Ev.onInput UpdateAccountSheetName
                    ]
                    []
            , Page.formElement "Expenses sheet name:" <|
                H.input
                    [ Attr.value expenseSheet
                    , Ev.onInput UpdateExpenseSheetName
                    ]
                    []
            , H.div [ Attr.class "flex justify-around gap-4" ]
                [ H.button
                    [ Attr.class "primary"
                    , Ev.onClick SaveSheetSettings
                    ]
                    [ H.text "Save" ]
                , H.button
                    [ Ev.onClick Reset
                    ]
                    [ H.text "Reset" ]
                ]
            ]


saveSheetSettings : Model -> Cmd Msg
saveSheetSettings model =
    Cmd.batch
        [ Capacitor.saveToStorage ( "sheetId", JsonE.string model.sheetId )
        , Capacitor.saveToStorage ( "accountSheet", JsonE.string model.accountSheet )
        , Capacitor.saveToStorage ( "expenseSheet", JsonE.string model.expenseSheet )
        ]
