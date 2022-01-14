port module Auth exposing (..)

import Capacitor
import Http
import Json.Decode as JsonD
import Json.Encode as JsonE


type Msg
    = GetToken String
    | ReceiveAuthCode String
    | ReceiveRefreshToken String
    | GotToken (Result Http.Error AuthObject)


type alias Model =
    { token : String
    , authState : AuthState
    }


type AuthState
    = NotLoggedIn
    | GettingAccessToken
    | LoggedIn
    | AuthError String


init =
    Model "" NotLoggedIn


type alias AuthObject =
    { refreshToken : String
    , token : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetToken rt ->
            ( { model | authState = GettingAccessToken }, getNewAccessTokenFromRefreshToken rt )

        ReceiveAuthCode code ->
            ( { model | authState = GettingAccessToken }
            , getAccessTokenFromAuthCode code
            )

        ReceiveRefreshToken token ->
            if token == "" then
                ( model, Cmd.none )

            else
                ( { model | authState = GettingAccessToken }
                , getNewAccessTokenFromRefreshToken token
                )

        GotToken res ->
            case res of
                Ok authObj ->
                    ( { model
                        | token = authObj.token
                        , authState = LoggedIn
                      }
                    , Capacitor.saveToStorage ( "refreshToken", JsonE.string authObj.refreshToken )
                    )

                Err e ->
                    ( { model | authState = AuthError (Debug.toString e) }, Capacitor.showAlert { title = "Error", message = "Something went wrong when trying to authorize: " ++ Debug.toString e } )


authUrl =
    "https://oauth2.googleapis.com/token"


clientId =
    "34596233405-fmkeqrgsajsuhd39bruih2jsh7ahoq8j.apps.googleusercontent.com"


redirectURI =
    "com.druchan.xpnsadd:/"


scopes =
    "https://www.googleapis.com/auth/spreadsheets"


url =
    "https://accounts.google.com/o/oauth2/v2/auth?client_id=" ++ clientId ++ "&redirect_uri=" ++ redirectURI ++ "&response_type=code&scope=" ++ scopes


getNewAccessTokenFromRefreshToken : String -> Cmd Msg
getNewAccessTokenFromRefreshToken refreshToken =
    let
        decoder =
            JsonD.map (\at -> AuthObject refreshToken at) (JsonD.field "access_token" JsonD.string)
    in
    Http.post
        { url = authUrl
        , body =
            Http.jsonBody
                (JsonE.object
                    [ ( "client_id", JsonE.string clientId )
                    , ( "grant_type", JsonE.string "refresh_token" )
                    , ( "refresh_token", JsonE.string refreshToken )
                    ]
                )
        , expect = Http.expectJson GotToken decoder
        }


decoder1 : JsonD.Decoder AuthObject
decoder1 =
    JsonD.map2 AuthObject
        (JsonD.field "refresh_token" JsonD.string)
        (JsonD.field "access_token" JsonD.string)


getAccessTokenFromAuthCode : String -> Cmd Msg
getAccessTokenFromAuthCode code =
    Http.post
        { url = authUrl
        , body =
            Http.jsonBody
                (JsonE.object
                    [ ( "client_id", JsonE.string clientId )
                    , ( "grant_type", JsonE.string "authorization_code" )
                    , ( "redirect_uri", JsonE.string redirectURI )
                    , ( "code", JsonE.string code )
                    ]
                )
        , expect = Http.expectJson GotToken decoder1
        }


initiateLogin : () -> Cmd msg
initiateLogin _ =
    doLogin url



-- ports


port doLogin : String -> Cmd msg


port receiveAuthCode : (String -> msg) -> Sub msg


port receiveRefreshToken : (String -> msg) -> Sub msg


port checkForRefreshToken : () -> Cmd msg
