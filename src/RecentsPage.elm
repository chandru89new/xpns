module RecentsPage exposing (..)

import API
import Dict
import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Http
import Json.Decode as JsonD
import Page


type Msg
    = LoadTransactions
    | TransactionsLoaded (Result API.Error (List (List String)))
    | GoToHomePage


type Transactions
    = Transactions (List (List String))
    | ErrorMsg String
    | Loading


type alias Model =
    { transactions : Transactions
    }


init : Model
init =
    { transactions = Loading }


update : Page.Global -> Msg -> Model -> ( Model, Cmd Msg )
update globals msg model =
    case msg of
        LoadTransactions ->
            ( { model | transactions = Loading }, loadTransactions globals )

        GoToHomePage ->
            ( model, Cmd.none )

        TransactionsLoaded res ->
            case res of
                Err e ->
                    ( { model | transactions = ErrorMsg <| API.errorToString e }, Cmd.none )

                Ok list ->
                    ( { model | transactions = Transactions (list |> List.reverse |> List.take 50) }, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ Page.renderHeader
            { icon = FeatherIcons.home
            , text = "Recent Txns"
            , msg = GoToHomePage
            }
        , Page.renderBody <|
            renderBody model
        ]


renderBody : Model -> Html Msg
renderBody { transactions } =
    case transactions of
        Transactions txns ->
            H.div
                [ Attr.class "flex flex-col gap-10"
                ]
            <|
                List.map txnCard txns

        Loading ->
            H.div [] [ H.text "Loading recent transactions..." ]

        ErrorMsg str ->
            Page.showError str


loadTransactions : Page.Global -> Cmd Msg
loadTransactions { token, sheetId, expenseSheet } =
    let
        expenseSheet_ =
            Maybe.withDefault Page.expenseSheetDefault expenseSheet

        decoder =
            JsonD.field "values" (JsonD.list (JsonD.list JsonD.string))

        baseURL =
            "https://sheets.googleapis.com/v4/spreadsheets/" ++ (sheetId |> Maybe.withDefault "") ++ "/values/" ++ expenseSheet_ ++ "!A2:Z"

        expect =
            API.expectJson
                TransactionsLoaded
                decoder

        queryParams =
            []
    in
    case sheetId of
        Nothing ->
            Page.msgToCmd <| TransactionsLoaded (Result.Err <| API.BadUrl "No sheet ID set")

        _ ->
            API.get token baseURL queryParams expect


txnCard : List String -> Html msg
txnCard txn =
    let
        withDefault =
            Maybe.withDefault ""

        dict =
            List.indexedMap (\idx val -> ( String.fromInt idx, val )) txn |> Dict.fromList

        date =
            Dict.get "0" dict |> withDefault

        note =
            Dict.get "1" dict |> withDefault

        xpns =
            Dict.get "3" dict |> withDefault

        acc =
            Dict.get "4" dict |> withDefault
    in
    H.div []
        [ H.div [ Attr.class "text-xs text-gray-500" ] [ H.text <| date ++ " | " ++ acc ]
        , H.div [] [ H.text xpns ]
        , H.div [ Attr.class "text-xs text-gray-500" ] [ H.text note ]
        ]
