port module Main exposing (..)

import API
import AccountsPage
import Auth
import Browser
import ExpenseTracker exposing (clearExpenseTrackerData)
import FeatherIcons as Icons
import HomePage
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import IncomePage
import Json.Decode as JsonD
import Page
import Ports
import Process
import RecentsPage
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
    , sheetSettings : SettingsPage.Model
    , sheetError : String
    , accountsLoading : Bool
    , toastMsg : Maybe String
    , recentsPage : RecentsPage.Model
    , accountsPage : AccountsPage.Model
    , authError : Maybe String
    }


type Page
    = Loading
    | Login
    | HomePage
    | ExpenseTrackerPage
    | SettingsPage
    | TransferPage
    | IncomePage
    | RecentsPage
    | AccountsPage


type Msg
    = NoOp
    | DoLogin
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
    | ReceiveSheetSettingsFromStorage ( String, String, String )
    | Logout
    | RecentsPageMsg RecentsPage.Msg
    | AccountsPageMsg AccountsPage.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    Tuple.pair
        { currentPage = Login
        , homePage = HomePage.init
        , expenseTracker = ExpenseTracker.init
        , accounts = []
        , auth = Auth.init
        , sheetSettings = SettingsPage.init
        , sheetError = ""
        , transferPage = TransferPage.init
        , toastMsg = Nothing
        , incomePage = IncomePage.init
        , recentsPage = RecentsPage.init
        , accountsPage = AccountsPage.init
        , accountsLoading = False
        , authError = Nothing

        --sheetId = "1E-XVfWRerpSjdtey0U4NdVL2A00NBacLvmGgHTX6VeU"
        }
        (Cmd.batch
            [ Cmd.map AuthMsg <| Auth.checkForRefreshToken ()
            , Ports.getSheetSettingsFromStorage ()
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

                RecentsPage ->
                    ( { model | currentPage = RecentsPage, recentsPage = RecentsPage.init }, Cmd.map RecentsPageMsg <| Page.msgToCmd RecentsPage.LoadTransactions )

                IncomePage ->
                    ( { model | currentPage = IncomePage }, Cmd.none )

                TransferPage ->
                    ( { model | currentPage = TransferPage }, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Login ->
                    ( model, Cmd.none )

                AccountsPage ->
                    ( { model | currentPage = AccountsPage, accountsPage = AccountsPage.init }
                    , Cmd.map AccountsPageMsg <| Page.msgToCmd AccountsPage.LoadAccounts
                    )

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
                        { model | auth = m, currentPage = Login, authError = Just "Something went wrong. You need to login again." }
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
                        { model | auth = m, currentPage = HomePage, accountsLoading = True, authError = Nothing }
                        (Cmd.batch
                            [ cmd_
                            , getAccounts
                                { token =
                                    emptyStringToMaybe m.token
                                , sheetId = model.sheetSettings.sheetId
                                , accountSheet = Maybe.withDefault Page.accountsSheetDefault model.sheetSettings.accountSheet
                                }
                            ]
                        )

        HomePageMsg homePageMsg ->
            case homePageMsg of
                HomePage.GoToExpensePage ->
                    update (GoTo ExpenseTrackerPage) model

                HomePage.GoToTransferPage ->
                    update (GoTo TransferPage) model

                HomePage.GoToSettingsPage ->
                    update (GoTo SettingsPage) model

                HomePage.GoToIncomePage ->
                    update (GoTo IncomePage) model

                HomePage.GoToRecentsPage ->
                    update (GoTo RecentsPage) model

                HomePage.Logout ->
                    update Logout model

                HomePage.GoToAccountsPage ->
                    update (GoTo AccountsPage) model

        ExpenseTrackerPageMsg expenseTrackerMsg ->
            let
                ( m, cmd ) =
                    ExpenseTracker.update expenseTrackerMsg model.expenseTracker { token = model.auth.token, sheetId = model.sheetSettings.sheetId, expenseSheet = model.sheetSettings.expenseSheet }

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
                            ( { model | expenseTracker = m }, Cmd.batch [ cmd_, hideToastMessage 5 ] )

                _ ->
                    Tuple.pair
                        { model | expenseTracker = m }
                        cmd_

        SettingsPageMsg settingsPageMsg ->
            let
                ( m, cmd ) =
                    SettingsPage.update settingsPageMsg model.sheetSettings

                newModel =
                    { model | sheetSettings = m }

                newCmd =
                    Cmd.map SettingsPageMsg cmd
            in
            case settingsPageMsg of
                SettingsPage.GoToHomePage ->
                    update (GoTo HomePage) newModel

                SettingsPage.SaveSheetSettings ->
                    ( { newModel | toastMsg = Just "Saved!", accountsLoading = True }
                    , Cmd.batch
                        [ newCmd
                        , getAccounts
                            { token = emptyStringToMaybe model.auth.token
                            , sheetId = m.sheetId
                            , accountSheet = Maybe.withDefault Page.accountsSheetDefault m.accountSheet
                            }
                        , hideToastMessage 5
                        ]
                    )

                _ ->
                    ( newModel, newCmd )

        GotAccounts res ->
            case res of
                Ok accs ->
                    ( { model | accountsLoading = False, accounts = accs |> Set.fromList |> Set.toList, sheetError = "" }, Cmd.none )

                Err _ ->
                    ( { model | accountsLoading = False, accounts = [], sheetError = "Could not get accounts for the given sheetID/account sheet name combination. Please check if the sheetID and the accounts sheet name are correct." }
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

        Logout ->
            ( { model
                | accounts = []
                , auth = Auth.init
                , currentPage = Login
              }
            , logOut ()
            )

        ReceiveSheetSettingsFromStorage ( sheetId, accountSheet, expenseSheet ) ->
            let
                sheetId_ =
                    if String.trim sheetId == "" then
                        Nothing

                    else
                        Just sheetId

                accountSheet_ =
                    if String.trim accountSheet == "" then
                        Nothing

                    else
                        Just accountSheet

                expenseSheet_ =
                    if String.trim expenseSheet == "" then
                        Nothing

                    else
                        Just expenseSheet

                sheetSettings =
                    SettingsPage.Model
                        sheetId_
                        accountSheet_
                        expenseSheet_
            in
            ( { model | sheetSettings = sheetSettings, accountsLoading = True }
            , Cmd.batch
                [ getAccounts
                    { token = emptyStringToMaybe model.auth.token
                    , sheetId = sheetId_
                    , accountSheet = Maybe.withDefault Page.accountsSheetDefault accountSheet_
                    }
                ]
            )

        RecentsPageMsg recentsPageMsg ->
            let
                ( m, cmd ) =
                    RecentsPage.update (getGlobals model) recentsPageMsg model.recentsPage

                cmd_ =
                    Cmd.map RecentsPageMsg cmd

                m_ =
                    { model | recentsPage = m }
            in
            case recentsPageMsg of
                RecentsPage.GoToHomePage ->
                    ( { m_ | currentPage = HomePage }, cmd_ )

                _ ->
                    ( m_, cmd_ )

        AccountsPageMsg accPageMsg ->
            let
                ( m, cmd ) =
                    AccountsPage.update (getGlobals model) accPageMsg model.accountsPage

                cmd_ =
                    Cmd.map AccountsPageMsg cmd

                m_ =
                    { model | accountsPage = m }
            in
            case accPageMsg of
                AccountsPage.GoToHomePage ->
                    ( { m_ | currentPage = HomePage }, cmd_ )

                _ ->
                    ( m_, cmd_ )


subscriptions _ =
    Sub.batch
        [ Sub.map AuthMsg <| Auth.receiveRefreshToken Auth.ReceiveRefreshToken
        , Sub.map AuthMsg <| Auth.receiveAuthCode Auth.ReceiveAuthCode
        , receiveSheetSettingsFromStorage ReceiveSheetSettingsFromStorage
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
                    viewLoginPage model

                HomePage ->
                    HomePage.view |> H.map HomePageMsg

                ExpenseTrackerPage ->
                    ExpenseTracker.view (getGlobals model) model.expenseTracker
                        |> H.map ExpenseTrackerPageMsg

                SettingsPage ->
                    SettingsPage.view model.sheetSettings |> H.map SettingsPageMsg

                TransferPage ->
                    TransferPage.view (getGlobals model) model.transferPage |> H.map TransferPageMsg

                IncomePage ->
                    IncomePage.view (getGlobals model) model.incomePage |> H.map IncomePageMsg

                RecentsPage ->
                    RecentsPage.view model.recentsPage
                        |> H.map RecentsPageMsg

                AccountsPage ->
                    AccountsPage.view model.accountsPage
                        |> H.map AccountsPageMsg
    in
    H.div []
        [ pageContent
        , Page.viewToaster model.toastMsg

        -- , renderDebug model
        ]


viewLoginPage : Model -> Html Msg
viewLoginPage { authError } =
    H.div
        [ Attr.class "flex flex-col h-screen items-center gap-10 justify-center"
        ]
        [ H.div [ Attr.class "gap-2 flex flex-col items-center" ]
            [ Page.renderIcon { icon = Icons.user, size = 42 }
            , H.div [] [ H.text <| Maybe.withDefault "" authError ]
            ]
        , H.button
            [ Attr.class "primary"
            , Ev.onClick DoLogin
            ]
            [ H.text "Login" ]
        ]


getAccounts : { token : Maybe String, sheetId : Maybe String, accountSheet : String } -> Cmd Msg
getAccounts { token, sheetId, accountSheet } =
    let
        decoder =
            JsonD.field "values" (JsonD.list (JsonD.list JsonD.string))
                |> JsonD.map (List.head >> Maybe.withDefault [])
                |> JsonD.map (List.drop 1)

        baseURL =
            Maybe.map (\id -> "https://sheets.googleapis.com/v4/spreadsheets/" ++ id ++ "/values/" ++ accountSheet ++ "!A:A") sheetId

        expect =
            Http.expectJson GotAccounts decoder

        queryParams =
            [ ( "majorDimension", "COLUMNS" ) ]
    in
    case token of
        Just token_ ->
            case baseURL of
                Just url ->
                    API.get token_ url queryParams expect

                Nothing ->
                    Page.msgToCmd <| GotAccounts <| Result.Err <| Http.BadUrl "No sheet ID"

        Nothing ->
            Cmd.none


getGlobals : Model -> Page.Global
getGlobals model =
    { accounts = model.accounts
    , sheetError = model.sheetError
    , sheetId = model.sheetSettings.sheetId
    , token = model.auth.token
    , accountSheet = model.sheetSettings.accountSheet
    , expenseSheet = model.sheetSettings.expenseSheet
    , accountsLoading = model.accountsLoading
    , authError = model.authError
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


port receiveSheetSettingsFromStorage : (( String, String, String ) -> msg) -> Sub msg


port logOut : () -> Cmd msg


port goBack : (String -> msg) -> Sub msg


emptyStringToMaybe : String -> Maybe String
emptyStringToMaybe str =
    if String.trim str == "" then
        Nothing

    else
        Just str
