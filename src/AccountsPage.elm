module AccountsPage exposing (..)

import API
import Dict
import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Json.Decode as JsonD
import Maybe exposing (withDefault)
import Page exposing (accountsSheetDefault)


type Msg
    = GoToHomePage
    | LoadAccounts
    | AccountsLoaded (Result API.Error (List (List String)))


type Accounts
    = Loading
    | Accounts (List Account)
    | ErrorMsg String


type alias Model =
    { accounts : Accounts }


type alias Account =
    { id : String
    , name : String
    , balance : String
    , type_ : String
    }


init =
    Model Loading


update : Page.Global -> Msg -> Model -> ( Model, Cmd Msg )
update globals msg model =
    case msg of
        LoadAccounts ->
            ( { model | accounts = Loading }, loadAccounts globals )

        AccountsLoaded res ->
            case res of
                Err e ->
                    ( { model | accounts = ErrorMsg <| API.errorToString e }, Cmd.none )

                Ok accs ->
                    let
                        withDefault =
                            Maybe.withDefault ""

                        toAccount : List String -> Account
                        toAccount list =
                            let
                                dict =
                                    List.indexedMap (\idx val -> ( String.fromInt idx, val )) list |> Dict.fromList
                            in
                            { id = Dict.get "0" dict |> withDefault
                            , name = Dict.get "1" dict |> withDefault
                            , balance = Dict.get "2" dict |> withDefault
                            , type_ = Dict.get "3" dict |> withDefault
                            }
                    in
                    ( { model | accounts = Accounts <| List.map toAccount accs }, Cmd.none )

        GoToHomePage ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ Page.renderHeader
            { icon = FeatherIcons.home
            , text = "Account Balances"
            , msg = GoToHomePage
            }
        , Page.renderBody <|
            renderBody model
        ]


renderBody : Model -> Html Msg
renderBody { accounts } =
    case accounts of
        Loading ->
            H.div [] [ H.text "Loading..." ]

        ErrorMsg msg ->
            Page.showError msg

        Accounts accs ->
            H.div
                [ Attr.class "flex flex-col gap-10"
                ]
            <|
                List.map accountCard accs


loadAccounts : Page.Global -> Cmd Msg
loadAccounts { token, sheetId, accountSheet } =
    let
        accountSheet_ =
            Maybe.withDefault accountsSheetDefault accountSheet

        decoder =
            JsonD.field "values" (JsonD.list (JsonD.list JsonD.string))

        baseURL =
            Maybe.map (\id -> "https://sheets.googleapis.com/v4/spreadsheets/" ++ id ++ "/values/" ++ accountSheet_ ++ "!A2:Z") sheetId

        expect =
            API.expectJson
                AccountsLoaded
                decoder

        queryParams =
            []
    in
    case baseURL of
        Just url ->
            API.get token url queryParams expect

        Nothing ->
            AccountsLoaded (Result.Err <| API.BadUrl "No sheet ID set.") |> Page.msgToCmd


accountCard : Account -> Html msg
accountCard acc =
    H.div [ Attr.class "flex flex-col" ]
        [ H.div [ Attr.class "text-gray-400 text-xs" ] [ H.text <| "id : " ++ acc.id ]
        , H.div [ Attr.class "text-lg" ] [ H.text acc.balance ]
        , H.div [ Attr.class "text-xs" ]
            [ H.span [] [ H.text acc.name ]
            , if String.length acc.type_ > 0 then
                H.span [ Attr.class "text-gray-500" ] [ H.text <| " (type : " ++ acc.type_ ++ ")" ]

              else
                H.span [] []
            ]
        ]
