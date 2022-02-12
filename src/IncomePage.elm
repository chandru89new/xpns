module IncomePage exposing (..)

import API
import Capacitor
import ExpenseTracker
import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Encode as JsonE
import Page
import Result
import Task


type alias Model =
    { amount : String
    , intoAccount : String
    , info : String
    , date : String
    , notes : String
    , pageState : PageState
    }


type PageState
    = Loading
    | Saving
    | Loaded


init =
    { amount = ""
    , intoAccount = ""
    , info = ""
    , date = ""
    , notes = ""
    , pageState = Loaded
    }


type Msg
    = NoOp
    | GoToHomePage
    | UpdateAmount String
    | UpdateIntoAccount String
    | UpdateInfo String
    | UpdateDate String
    | UpdateNotes String
    | SaveIncome
    | SaveTransferDone (Result Http.Error ())


update : Page.Global -> Msg -> Model -> ( Model, Cmd Msg )
update globals msg model =
    case msg of
        GoToHomePage ->
            ( init, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        UpdateDate date ->
            ( { model | date = date }, Cmd.none )

        UpdateIntoAccount acc ->
            ( { model | intoAccount = acc }, Cmd.none )

        UpdateAmount amt ->
            ( { model | amount = amt }, Cmd.none )

        UpdateNotes notes ->
            ( { model | notes = notes }, Cmd.none )

        UpdateInfo info ->
            ( { model | info = info }, Cmd.none )

        SaveIncome ->
            ( model, saveIncome globals model )

        SaveTransferDone res ->
            case res of
                Ok _ ->
                    ( init
                    , Task.perform (\_ -> GoToHomePage) (Task.succeed ())
                    )

                Err e ->
                    ( model
                    , Capacitor.showAlert
                        { title = " Error "
                        , message = Page.errToString e
                        }
                    )


view : Page.Global -> Model -> Html Msg
view globals model =
    pageRenderer <|
        H.div
            [ Attr.class "flex flex-col gap-5"
            ]
            [ if globals.sheetError /= "" then
                ExpenseTracker.showError globals.sheetError

              else if globals.sheetId == Nothing then
                ExpenseTracker.showError "No sheet ID."

              else
                H.text ""
            , Page.formElement "How much?" <|
                H.input
                    [ Attr.value model.amount
                    , Ev.onInput UpdateAmount
                    , Attr.type_ "number"
                    ]
                    []
            , Page.formElement "To which account?" <|
                H.select
                    [ Ev.onInput UpdateIntoAccount
                    , Attr.placeholder "Select account"
                    ]
                <|
                    List.map
                        (\option ->
                            H.option [ Attr.value option, Attr.selected (option == model.intoAccount) ]
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
            , Page.formElement "When?" <|
                H.input
                    [ Attr.value model.date
                    , Ev.onInput UpdateDate
                    , Attr.type_ "date"
                    ]
                    []
            , Page.formElement "Deets?" <|
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
                    , Ev.onClick SaveIncome
                    ]
                    [ H.text "Save" ]
                ]
            ]


pageRenderer : Html Msg -> Html Msg
pageRenderer content =
    H.div [] [ pageHeader, Page.renderBody content ]


pageHeader : Html Msg
pageHeader =
    Page.renderHeader
        { text = "Income"
        , icon = FeatherIcons.home
        , msg = GoToHomePage
        }


isFormValid : Model -> Bool
isFormValid model =
    let
        notEmpty =
            not <| List.any (String.trim >> String.isEmpty) [ model.date, model.info, model.intoAccount, model.amount ]

        amountIsValidNumber =
            case String.toFloat model.amount of
                Nothing ->
                    False

                _ ->
                    True
    in
    notEmpty && amountIsValidNumber


saveIncome : Page.Global -> Model -> Cmd Msg
saveIncome { token, sheetId, expenseSheet } model =
    let
        body =
            Http.jsonBody <|
                JsonE.object
                    [ ( "values"
                      , JsonE.list (JsonE.list JsonE.string)
                            [ [ model.date
                              , model.info
                              , "income"
                              , "-" ++ model.amount
                              , model.intoAccount
                              , model.notes
                              ]
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
            Result.Err
                (Http.BadUrl "No sheet ID set")
                |> SaveTransferDone
                |> Page.msgToCmd

        Just url ->
            API.post token url queryParams body expect
