module ExpenseTracker exposing (..)

import API
import Capacitor
import FeatherIcons
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Encode as JsonE
import Page exposing (formElement)
import Task


type alias Model =
    { amount : String
    , account : String
    , info : String
    , notes : String
    , date : String
    , category : String
    , pageState : PageState
    , allAccounts : List String
    }


type PageState
    = Loading
    | Loaded
    | Saving


init : Model
init =
    { account = ""
    , amount = ""
    , info = ""
    , notes = ""
    , date = ""
    , category = ""
    , allAccounts = []
    , pageState = Loading
    }


type Msg
    = UpdateAmount String
    | UpdateInfo String
    | UpdateAccount String
    | UpdateCategory String
    | UpdateDate String
    | UpdateNotes String
    | SaveExpense
    | SaveExpenseResponded (Result Http.Error ())
    | GoToHomePage


type alias Global a =
    { a
        | token : String
        , sheetId : Maybe String
        , expenseSheet : String
    }


update : Msg -> Model -> Global a -> ( Model, Cmd Msg )
update msg model { token, sheetId, expenseSheet } =
    case msg of
        UpdateAccount acc ->
            ( { model | account = acc }, Cmd.none )

        UpdateAmount amt ->
            ( { model | amount = amt }, Cmd.none )

        UpdateCategory cat ->
            ( { model | category = cat }, Cmd.none )

        UpdateDate date ->
            ( { model | date = date }, Cmd.none )

        UpdateInfo info ->
            ( { model | info = info }, Cmd.none )

        UpdateNotes n ->
            ( { model | notes = n }, Cmd.none )

        SaveExpense ->
            if sheetId == Nothing then
                ( model, Cmd.none )

            else
                ( { model | pageState = Saving }, saveExpense model { sheetId = sheetId, token = token, expenseSheet = expenseSheet } )

        SaveExpenseResponded response ->
            case response of
                Ok _ ->
                    ( clearExpenseTrackerData model, Task.perform (\_ -> GoToHomePage) (Task.succeed ()) )

                Err e ->
                    ( model
                    , Capacitor.showAlert
                        { title = " Error "
                        , message = Page.errToString e
                        }
                    )

        _ ->
            ( model, Cmd.none )


pageWrapper : Html Msg -> Html Msg
pageWrapper body =
    H.div []
        [ Page.renderHeader
            { text = "Expense"
            , icon = FeatherIcons.home
            , msg = GoToHomePage
            }
        , Page.renderBody body
        ]


view : Page.Global -> Model -> Html Msg
view { sheetError, accounts } model =
    case model.pageState of
        Loading ->
            Page.pageLoaderDiv

        Loaded ->
            pageWrapper <|
                if sheetError /= "" then
                    H.div [] [ showError sheetError, viewForm accounts model ]

                else
                    viewForm accounts model

        Saving ->
            pageWrapper <| H.div [] [ viewForm accounts model, Page.viewToaster <| Just "Saving..." ]


viewForm : List String -> Model -> Html Msg
viewForm accounts model =
    H.div []
        [ H.div
            [ Attr.class "flex flex-col gap-10"
            ]
          <|
            [ formElement "How much?" <|
                H.input
                    [ Attr.type_ "number"
                    , Attr.value model.amount
                    , Ev.onInput UpdateAmount
                    ]
                    []
            , formElement "What did you spend on?" <|
                H.input
                    [ Attr.value model.info
                    , Ev.onInput UpdateInfo
                    ]
                    []
            , formElement "When did you spend this?" <|
                H.input
                    [ Attr.type_ "date"
                    , Attr.value model.date
                    , Ev.onInput UpdateDate
                    ]
                    []
            , formElement "Which account from?" <|
                H.select
                    [ Ev.onInput UpdateAccount
                    ]
                    (List.map
                        (\account ->
                            H.option
                                [ Attr.value account
                                , Attr.selected (account == model.account)
                                ]
                                [ H.text
                                    (if account == "" then
                                        "Select account"

                                     else
                                        account
                                    )
                                ]
                        )
                     <|
                        List.concat [ [ "" ], accounts ]
                    )

            -- H.div []
            --     [ H.input
            --         [ Attr.list "accounts"
            --         , Attr.value model.account
            --         ]
            --         []
            --     , H.datalist
            --         [ Attr.id "accounts"
            --         ]
            --       <|
            --         List.map
            --             (\account ->
            --                 H.option
            --                     [ Attr.value account
            --                     , Attr.selected (account == model.account)
            --                     ]
            --                     [ H.text
            --                         (if account == "" then
            --                             "Select account"
            --                          else
            --                             account
            --                         )
            --                     ]
            --             )
            --         <|
            --             List.concat [ [ "" ], accounts ]
            --     ]
            , formElement "Category? (optional)" <|
                H.input
                    [ Attr.value model.category
                    , Ev.onInput UpdateCategory
                    ]
                    []
            , formElement "Notes (optional)" <|
                H.textarea
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
                        ((not <| isExpenseFormValid model) || model.pageState == Saving)
                    , Ev.onClick SaveExpense
                    ]
                    [ H.text "Save" ]
                ]
            ]
        ]


isExpenseFormValid : Model -> Bool
isExpenseFormValid expense =
    let
        nothingIsEmpty =
            [ expense.amount, expense.date, expense.info, expense.account ]
                |> List.all (\str -> String.length str > 0)

        amountIsValidNumber =
            case String.toFloat expense.amount of
                Nothing ->
                    False

                _ ->
                    True
    in
    nothingIsEmpty && amountIsValidNumber


showError : String -> Html msg
showError error =
    H.div
        [ Attr.class "p-2 rounded bg-red-100 text-xs mb-5"
        ]
        [ H.text error ]


saveExpense : Model -> Global a -> Cmd Msg
saveExpense model { sheetId, token, expenseSheet } =
    let
        body =
            Http.jsonBody <|
                JsonE.object
                    [ ( "values", JsonE.list (JsonE.list JsonE.string) [ [ model.date, model.info, model.category, model.amount, model.account, model.notes ] ] )
                    ]

        queryParams =
            [ ( "valueInputOption", "USER_ENTERED" )
            ]

        expect =
            Http.expectWhatever SaveExpenseResponded

        baseURL =
            Maybe.map (\id -> "https://sheets.googleapis.com/v4/spreadsheets/" ++ id ++ "/values/" ++ expenseSheet ++ "!A:Z" ++ ":append") sheetId
    in
    case baseURL of
        Just url ->
            API.post token url queryParams body expect

        Nothing ->
            Page.msgToCmd <| SaveExpenseResponded <| Result.Err <| Http.BadUrl "No sheet ID set."


clearExpenseTrackerData : Model -> Model
clearExpenseTrackerData expense =
    Model "" expense.account "" "" "" "" Loaded expense.allAccounts
