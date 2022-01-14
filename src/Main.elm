port module Main exposing (..)

import API
import Auth
import Browser
import Capacitor
import ExpenseTracker exposing (clearExpenseTrackerData)
import FeatherIcons as Icons
import HomePage
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import IncomePage
import Json.Decode as JsonD
import Json.Encode as JsonE
import Page
import Process
import Set
import SettingsPage
import Task
import TransferPage


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { currentPage : Page
    , homePage : HomePage.Model
    , expenseTracker : ExpenseTracker.Model
    , accounts : List String
    , transferPage : TransferPage.Model
    , incomePage : IncomePage.Model
    , auth : Auth.Model
    , sheetId : String
    , sheetError : String
    , toastMsg : Maybe String
    }


type Page
    = Loading
    | Login
    | HomePage
    | ExpenseTrackerPage
    | SettingsPage
    | TransferPage
    | IncomePage
    | GlobalError


type Msg
    = NoOp
    | DoLogin
    | UpdateSheetId String
    | GoTo Page
    | GoBack
    | GotAccounts (Result Http.Error (List String))
    | SetToastMsg (Maybe String)
    | AuthMsg Auth.Msg
    | HomePageMsg HomePage.Msg
    | ExpenseTrackerPageMsg ExpenseTracker.Msg
    | TransferPageMsg TransferPage.Msg
    | SettingsPageMsg SettingsPage.Msg
    | IncomePageMsg IncomePage.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    Tuple.pair
        { currentPage = Loading
        , homePage = HomePage.init
        , expenseTracker = ExpenseTracker.init
        , accounts = []
        , auth = Auth.init
        , sheetId = ""
        , sheetError = ""
        , transferPage = TransferPage.init
        , toastMsg = Nothing
        , incomePage = IncomePage.init

        --sheetId = "1E-XVfWRerpSjdtey0U4NdVL2A00NBacLvmGgHTX6VeU"
        }
        (Cmd.batch
            [ Cmd.map AuthMsg <| Auth.checkForRefreshToken ()
            , getSheetIdFromStorage ()
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DoLogin ->
            ( model, Auth.initiateLogin () )

        GoTo page ->
            case page of
                HomePage ->
                    ( { model
                        | currentPage = HomePage
                        , expenseTracker = clearExpenseTrackerData model.expenseTracker
                      }
                    , Cmd.none
                    )

                ExpenseTrackerPage ->
                    ( { model | expenseTracker = ExpenseTracker.clearExpenseTrackerData model.expenseTracker, currentPage = ExpenseTrackerPage }, Cmd.none )

                SettingsPage ->
                    ( { model
                        | currentPage = SettingsPage
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AuthMsg authMsg ->
            let
                ( m, cmd ) =
                    Auth.update authMsg model.auth

                cmd_ =
                    Cmd.map AuthMsg cmd
            in
            case m.authState of
                Auth.AuthError _ ->
                    Tuple.pair
                        { model | auth = m, currentPage = GlobalError }
                        cmd_

                Auth.NotLoggedIn ->
                    Tuple.pair
                        { model | auth = m, currentPage = Login }
                        cmd_

                Auth.GettingAccessToken ->
                    Tuple.pair
                        { model | auth = m, currentPage = Loading }
                        cmd_

                Auth.LoggedIn ->
                    Tuple.pair
                        { model | auth = m, currentPage = HomePage }
                        (Cmd.batch
                            [ cmd_
                            , getAccounts
                                { token = m.token
                                , sheetId = model.sheetId
                                }
                            ]
                        )

        HomePageMsg homePageMsg ->
            case homePageMsg of
                HomePage.GoToExpensePage ->
                    update (GoTo ExpenseTrackerPage) model

                HomePage.GoToTransferPage ->
                    ( { model | currentPage = TransferPage }, Cmd.none )

                HomePage.GoToSettingsPage ->
                    ( { model | currentPage = SettingsPage }, Cmd.none )

                HomePage.GoToIncomePage ->
                    ( { model | currentPage = IncomePage }, Cmd.none )

                HomePage.Logout ->
                    ( model, logOut () )

        ExpenseTrackerPageMsg expenseTrackerMsg ->
            let
                ( m, cmd ) =
                    ExpenseTracker.update expenseTrackerMsg model.expenseTracker { token = model.auth.token, sheetId = model.sheetId }

                cmd_ =
                    Cmd.map ExpenseTrackerPageMsg <| cmd
            in
            case expenseTrackerMsg of
                ExpenseTracker.GoToHomePage ->
                    Tuple.pair
                        { model | expenseTracker = clearExpenseTrackerData model.expenseTracker, currentPage = HomePage }
                        cmd_

                ExpenseTracker.SaveExpenseResponded response ->
                    case response of
                        Ok _ ->
                            ( { model | expenseTracker = m, toastMsg = Just "Saved!" }
                            , Cmd.batch
                                [ cmd_
                                , hideToastMessage 5
                                ]
                            )

                        _ ->
                            ( { model | expenseTracker = m }, cmd_ )

                _ ->
                    Tuple.pair
                        { model | expenseTracker = m }
                        cmd_

        UpdateSheetId id ->
            Tuple.pair
                { model | sheetId = id, sheetError = "" }
            <|
                Cmd.batch
                    [ Capacitor.saveToStorage
                        ( "sheetId", JsonE.string id )
                    , if id /= "" && model.auth.token /= "" then
                        getAccounts { token = model.auth.token, sheetId = id }

                      else
                        Cmd.none
                    ]

        SettingsPageMsg settingsPageMsg ->
            case settingsPageMsg of
                SettingsPage.UpdateSheetId id ->
                    update (UpdateSheetId id) model

                SettingsPage.GoToHomePage ->
                    update (GoTo HomePage) model

        GotAccounts res ->
            case res of
                Ok accs ->
                    ( { model | accounts = accs |> Set.fromList |> Set.toList, sheetError = "" }, Cmd.none )

                Err _ ->
                    ( { model | accounts = [], sheetError = "Could not get accounts for the given sheetID. Please check if the sheetID is correct." }
                    , Cmd.batch
                        []
                    )

        SetToastMsg message ->
            ( { model | toastMsg = message }, hideToastMessage 5 )

        GoBack ->
            case model.currentPage of
                HomePage ->
                    ( model, exitApp () )

                ExpenseTrackerPage ->
                    ( { model
                        | currentPage = HomePage
                        , expenseTracker = clearExpenseTrackerData model.expenseTracker
                      }
                    , Cmd.none
                    )

                SettingsPage ->
                    ( { model | currentPage = HomePage }, Cmd.none )

                _ ->
                    ( { model | currentPage = HomePage }, Cmd.none )

        TransferPageMsg tpMsg ->
            let
                ( m, cmd ) =
                    TransferPage.update tpMsg model.transferPage (getGlobals model)

                cmd_ =
                    Cmd.map TransferPageMsg cmd
            in
            case tpMsg of
                TransferPage.GoToHomePage ->
                    ( { model | transferPage = TransferPage.clearForm model.transferPage, currentPage = HomePage }, cmd_ )

                TransferPage.SaveTransferDone res ->
                    case res of
                        Ok _ ->
                            ( { model | toastMsg = Just "Saved!" }
                            , Cmd.batch
                                [ cmd_
                                , hideToastMessage 5
                                ]
                            )

                        _ ->
                            ( { model | transferPage = m }, cmd_ )

                _ ->
                    ( { model | transferPage = m }, cmd_ )

        IncomePageMsg ipMsg ->
            let
                ( m, cmd ) =
                    IncomePage.update (getGlobals model) ipMsg model.incomePage

                cmd_ =
                    Cmd.map IncomePageMsg cmd
            in
            case ipMsg of
                IncomePage.GoToHomePage ->
                    ( { model | incomePage = m, currentPage = HomePage }, cmd_ )

                IncomePage.SaveTransferDone res ->
                    case res of
                        Ok _ ->
                            ( { model | incomePage = m, toastMsg = Just "Saved!" }
                            , Cmd.batch
                                [ cmd_
                                , hideToastMessage 5
                                ]
                            )

                        Err _ ->
                            ( { model | incomePage = m }, cmd_ )

                _ ->
                    ( { model | incomePage = m }, cmd_ )


subscriptions _ =
    Sub.batch
        [ Sub.map AuthMsg <| Auth.receiveRefreshToken Auth.ReceiveRefreshToken
        , Sub.map AuthMsg <| Auth.receiveAuthCode Auth.ReceiveAuthCode
        , receiveSheetIdFromStorage UpdateSheetId
        , goBack (\_ -> GoBack)
        ]


view : Model -> Html Msg
view model =
    let
        pageContent =
            case model.currentPage of
                Loading ->
                    Page.pageLoaderDiv

                Login ->
                    viewLoginPage

                HomePage ->
                    HomePage.view |> H.map HomePageMsg

                ExpenseTrackerPage ->
                    ExpenseTracker.view (Page.Global model.auth.token model.sheetId model.accounts model.sheetError) model.expenseTracker
                        |> H.map ExpenseTrackerPageMsg

                SettingsPage ->
                    SettingsPage.view model.sheetId |> H.map SettingsPageMsg

                TransferPage ->
                    TransferPage.view (getGlobals model) model.transferPage |> H.map TransferPageMsg

                IncomePage ->
                    IncomePage.view (getGlobals model) model.incomePage |> H.map IncomePageMsg

                GlobalError ->
                    H.div [] [ H.text "Catastrophic error page" ]
    in
    H.div []
        [ pageContent
        , Page.viewToaster model.toastMsg

        -- , renderDebug model
        ]


viewLoginPage : Html Msg
viewLoginPage =
    H.div
        [ Attr.class "flex flex-col h-screen items-center gap-10 justify-center"
        ]
        [ H.div [] [ Page.renderIcon { icon = Icons.user, size = 42 } ]
        , H.button
            [ Attr.class "primary"
            , Ev.onClick DoLogin
            ]
            [ H.text "Login" ]
        ]


renderDebug : Model -> Html Msg
renderDebug model =
    H.div
        [ Attr.class "p-3 text-xs overflow-hidden text-clip bg-black text-white"
        ]
        [ H.text <| Debug.toString model ]


getAccounts : { token : String, sheetId : String } -> Cmd Msg
getAccounts { token, sheetId } =
    let
        decoder =
            JsonD.field "values" (JsonD.list (JsonD.list JsonD.string))
                |> JsonD.map (List.head >> Maybe.withDefault [])

        baseURL =
            "https://sheets.googleapis.com/v4/spreadsheets/" ++ sheetId ++ "/values/" ++ "accounts!A:A"

        expect =
            Http.expectJson GotAccounts decoder

        queryParams =
            [ ( "majorDimension", "COLUMNS" ) ]
    in
    API.get token baseURL queryParams expect


getGlobals : Model -> Page.Global
getGlobals model =
    { accounts = model.accounts
    , sheetError = model.sheetError
    , sheetId = model.sheetId
    , token = model.auth.token
    }


hideToastMessage : Int -> Cmd Msg
hideToastMessage seconds =
    let
        sleeper =
            Process.sleep (toFloat (seconds * 1000)) |> Task.map (\_ -> Nothing)
    in
    Task.perform SetToastMsg sleeper



--- ports


port exitApp : () -> Cmd msg


port receiveSheetIdFromStorage : (String -> msg) -> Sub msg


port getSheetIdFromStorage : () -> Cmd msg


port logOut : () -> Cmd msg


port goBack : (String -> msg) -> Sub msg
