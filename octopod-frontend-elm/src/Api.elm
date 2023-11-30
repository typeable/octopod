module Api exposing (..)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Config exposing (AppAuth(..), AppUrl, Config)
import Http exposing (Error(..), Expect, expectStringResponse)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData)


get : Config -> (AppUrl -> Endpoint) -> Decoder a -> (Result Error a -> msg) -> Cmd msg
get config url decoder msg =
    Endpoint.request
        { method = "GET"
        , url = url config.appUrl
        , expect = expectJson msg decoder
        , headers = [ authHeader config.appAuth ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


put : Endpoint -> AppAuth -> Http.Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
put url auth body decoder msg =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = expectJson msg decoder
        , headers = [ authHeader auth ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


post : Config -> (AppUrl -> Endpoint) -> Http.Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
post config url body decoder msg =
    Endpoint.request
        { method = "POST"
        , url = url config.appUrl
        , expect = expectJson msg decoder
        , headers = [ authHeader config.appAuth ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


patch : Config -> (AppUrl -> Endpoint) -> Decoder a -> (Result Error a -> msg) -> Cmd msg
patch config url decoder msg =
    Endpoint.request
        { method = "PATCH"
        , url = url config.appUrl
        , expect = expectJson msg decoder
        , headers = [ authHeader config.appAuth ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Config -> (AppUrl -> Endpoint) -> Http.Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
delete config url body decoder msg =
    Endpoint.request
        { method = "DELETE"
        , url = url config.appUrl
        , expect = expectJson msg decoder
        , headers = [ authHeader config.appAuth ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String


type alias WebData a =
    RemoteData Error a


expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata.statusCode body)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (Decode.errorToString err))


authHeader : AppAuth -> Http.Header
authHeader (AppAuth appAuth) =
    Http.header "authorization" appAuth
