module TransferPage exposing (..)

import API
import Capacitor
import ExpenseTracker exposing (showError)
import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Encode as JsonE
import Page
import Task


type alias Model =
    { amount : String
    , fromAccount : String
    , toAccount : String
    , date : String
    , info : String
    , notes : String
    , pageState : PageState
    , transferType : TransferType
    }


type TransferType
    = Transfer
    | Investment


init =
    Model "" "" "" "" "" "" Loading Transfer


type PageState
    = Loading
    | Saving
    | Loaded


type Msg
    = GoToHomePage
    | UpdateFromAccount String
    | UpdateToAccount String
    | UpdateAmount String
    | UpdateNotes String
    | UpdateInfo String
    | UpdateDate String
    | ToggleTransferType Bool
    | SaveTransfer
    | SaveTransferDone (Result Http.Error ())


update : Msg -> Model -> Page.Global -> ( Model, Cmd Msg )
update msg model globals =
    case msg of
        GoToHomePage ->
            ( model, Cmd.none )

        UpdateFromAccount acc ->
            ( { model | fromAccount = acc }, Cmd.none )

        UpdateDate date ->
            ( { model | date = date }, Cmd.none )

        UpdateToAccount acc ->
            ( { model | toAccount = acc }, Cmd.none )

        UpdateAmount amt ->
            ( { model | amount = amt }, Cmd.none )

        UpdateNotes notes ->
            ( { model | notes = notes }, Cmd.none )

        UpdateInfo info ->
            ( { model | info = info }, Cmd.none )

        SaveTransfer ->
            ( { model | pageState = Saving }, saveTransfer model globals )

        SaveTransferDone res ->
            case res of
                Ok _ ->
                    ( clearForm model, Task.perform (\_ -> GoToHomePage) (Task.succeed ()) )

                Err e ->
                    ( model
                    , Capacitor.showAlert
                        { title = " Error "
                        , message = Page.errToString e
                        }
                    )

        ToggleTransferType _ ->
            let
                toggle ttype =
                    case ttype of
                        Transfer ->
                            Investment

                        Investment ->
                            Transfer
            in
            ( { model | transferType = toggle model.transferType }, Cmd.none )


pageHeader =
    Page.renderHeader
        { icon = FeatherIcons.home
        , msg = GoToHomePage
        , text = "Transfer"
        }


pageWrapper body =
    H.div []
        [ pageHeader
        , Page.renderBody body
        ]


view : Page.Global -> Model -> Html Msg
view globals model =
    pageWrapper <|
        H.div
            [ Attr.class "flex flex-col gap-5"
            ]
            [ if globals.sheetError /= "" then
                showError globals.sheetError

              else if globals.sheetId == Nothing then
                showError "No sheet ID."

              else
                H.text ""
            , Page.formElement "From which account?" <|
                H.select
                    [ Ev.onInput UpdateFromAccount
                    , Attr.placeholder "Select account"
                    ]
                <|
                    List.map
                        (\option ->
                            H.option [ Attr.value option, Attr.selected (option == model.fromAccount) ]
                                [ H.text
                                    (if option == "" then
                                        "Select account"

                                     else
                                        option
                                    )
                                ]
                        )
                    <|
                        List.concat [ [ "" ], globals.accounts ]
            , Page.formElement "To which account?" <|
                H.select
                    [ Ev.onInput UpdateToAccount
                    , Attr.placeholder "Select account"
                    ]
                <|
                    List.map
                        (\option ->
                            H.option [ Attr.value option, Attr.selected (option == model.toAccount) ]
                                [ H.text
                                    (if option == "" then
                                        "Select account"

                                     else
                                        option
                                    )
                                ]
                        )
                    <|
                        List.concat [ [ "" ], globals.accounts ]
            , Page.formElement "How much?" <|
                H.input
                    [ Attr.value model.amount
                    , Ev.onInput UpdateAmount
                    , Attr.type_ "number"
                    ]
                    []
            , Page.formElement "Investment?" <|
                H.div
                    [ Attr.class "flex items-center gap-2"
                    ]
                    [ H.input
                        [ Ev.onCheck ToggleTransferType
                        , Attr.checked (model.transferType == Investment)
                        , Attr.type_ "checkbox"
                        , Attr.id "transfer-type"
                        ]
                        []
                    , H.label
                        [ Attr.for "transfer-type"
                        ]
                        [ H.text "Yes, it's an investment" ]
                    ]
            , Page.formElement "When?" <|
                H.input
                    [ Attr.value model.date
                    , Ev.onInput UpdateDate
                    , Attr.type_ "date"
                    ]
                    []
            , Page.formElement "Deets? (optional)" <|
                H.input
                    [ Attr.value model.info
                    , Ev.onInput UpdateInfo
                    ]
                    []
            , Page.formElement "Notes (optional)" <|
                H.input
                    [ Attr.value model.notes
                    , Ev.onInput UpdateNotes
                    ]
                    []
            , H.div [ Attr.class "flex justify-around gap-4" ]
                [ H.button
                    [ Ev.onClick GoToHomePage
                    ]
                    [ H.text "Back" ]
                , H.button
                    [ Attr.class "primary"
                    , Attr.disabled
                        ((not <| isFormValid model) || model.pageState == Saving)
                    , Ev.onClick SaveTransfer
                    ]
                    [ H.text "Save" ]
                ]
            ]


isFormValid : Model -> Bool
isFormValid model =
    let
        dateIsValid =
            not <| String.isEmpty model.date

        modelAsNumber =
            case String.toFloat model.amount of
                Nothing ->
                    False

                Just _ ->
                    True

        accountsNotSame =
            model.toAccount /= model.fromAccount

        accountsValid =
            not <| String.isEmpty model.toAccount || String.isEmpty model.fromAccount
    in
    dateIsValid && modelAsNumber && accountsValid && accountsNotSame


saveTransfer : Model -> Page.Global -> Cmd Msg
saveTransfer model { token, expenseSheet, sheetId } =
    let
        body =
            Http.jsonBody <|
                JsonE.object
                    [ ( "values"
                      , JsonE.list (JsonE.list JsonE.string)
                            [ [ model.date
                              , model.info
                              , transferTypeToString model.transferType
                              , model.amount
                              , model.fromAccount
                              , model.notes
                              ]
                                |> List.map String.trim
                            , [ model.date
                              , model.info
                              , transferTypeToString model.transferType
                              , "-" ++ model.amount
                              , model.toAccount
                              , model.notes
                              ]
                                |> List.map String.trim
                            ]
                      )
                    ]

        queryParams =
            [ ( "valueInputOption", "USER_ENTERED" )
            ]

        expect =
            Http.expectWhatever SaveTransferDone

        baseURL =
            Maybe.map (\id -> "https://sheets.googleapis.com/v4/spreadsheets/" ++ id ++ "/values/" ++ expenseSheet ++ "!A:Z" ++ ":append") sheetId
    in
    case baseURL of
        Nothing ->
            Http.BadUrl "No sheet ID set"
                |> Result.Err
                |> SaveTransferDone
                |> Page.msgToCmd

        Just url ->
            API.post token url queryParams body expect


clearForm : Model -> Model
clearForm _ =
    { amount = ""
    , fromAccount = ""
    , toAccount = ""
    , date = ""
    , info = ""
    , notes = ""
    , pageState = Loaded
    , transferType = Transfer
    }


transferTypeToString : TransferType -> String
transferTypeToString ttype =
    case ttype of
        Transfer ->
            "transfer"

        Investment ->
            "investment"
