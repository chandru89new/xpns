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
    { sheetId : Maybe String
    , accountSheet : Maybe String
    , expenseSheet : Maybe String
    }


init =
    Model Nothing Nothing Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSheetId id ->
            case String.trim id of
                "" ->
                    ( { model | sheetId = Nothing }, Cmd.none )

                str ->
                    ( { model | sheetId = Just str }, Cmd.none )

        UpdateAccountSheetName name ->
            case String.trim name of
                "" ->
                    ( { model | accountSheet = Nothing }, Cmd.none )

                str ->
                    ( { model | accountSheet = Just str }, Cmd.none )

        UpdateExpenseSheetName name ->
            case String.trim name of
                "" ->
                    ( { model | expenseSheet = Nothing }, Cmd.none )

                str ->
                    ( { model | expenseSheet = Just str }, Cmd.none )

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
                    [ Attr.value (sheetId |> Maybe.withDefault "")
                    , Ev.onInput UpdateSheetId
                    , Attr.placeholder "eg. 1E-XVfWQerpTjdsey1U9NdVL4A10MJacLvmGgHTX6VeU"
                    ]
                    []
            , Page.formElement "Accounts sheet name:" <|
                H.input
                    [ Attr.value (Maybe.withDefault "" accountSheet)
                    , Ev.onInput UpdateAccountSheetName
                    , Attr.placeholder "using 'accounts' as default"
                    ]
                    []
            , Page.formElement "Expenses sheet name:" <|
                H.input
                    [ Attr.value (Maybe.withDefault "" expenseSheet)
                    , Ev.onInput UpdateExpenseSheetName
                    , Attr.placeholder "using 'expense' as default"
                    ]
                    []
            , H.div [ Attr.class "flex justify-around gap-4" ]
                [ H.button
                    [ Ev.onClick Reset
                    ]
                    [ H.text "Reset" ]
                , H.button
                    [ Attr.class "primary"
                    , Ev.onClick SaveSheetSettings
                    ]
                    [ H.text "Save" ]
                ]
            ]


saveSheetSettings : Model -> Cmd Msg
saveSheetSettings model =
    Cmd.batch
        [ Capacitor.saveToStorage ( "sheetId", JsonE.string (model.sheetId |> Maybe.withDefault "") )
        , Capacitor.saveToStorage ( "accountSheet", JsonE.string (Maybe.withDefault "" model.accountSheet) )
        , Capacitor.saveToStorage ( "expenseSheet", JsonE.string (Maybe.withDefault "" model.expenseSheet) )
        ]
