module Api exposing (..)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Config exposing (AppAuth(..), AppUrl, Config)
import Http
import Json.Decode exposing (Decoder)


get : Config -> (AppUrl -> Endpoint) -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
get config url decoder msg =
    Endpoint.request
        { method = "GET"
        , url = url config.appUrl
        , expect = Http.expectJson msg decoder
        , headers = [ authHeader config.appAuth ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


put : Endpoint -> AppAuth -> Http.Body -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
put url auth body decoder msg =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson msg decoder
        , headers = [ authHeader auth ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


post : Endpoint -> AppAuth -> Http.Body -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
post url auth body decoder msg =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson msg decoder
        , headers = [ authHeader auth ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


patch : Config -> (AppUrl -> Endpoint) -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
patch config url decoder msg =
    Endpoint.request
        { method = "PATCH"
        , url = url config.appUrl
        , expect = Http.expectJson msg decoder
        , headers = [ authHeader config.appAuth ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Config -> (AppUrl -> Endpoint) -> Http.Body -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
delete config url body decoder msg =
    Endpoint.request
        { method = "DELETE"
        , url = url config.appUrl
        , expect = Http.expectJson msg decoder
        , headers = [ authHeader config.appAuth ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


authHeader : AppAuth -> Http.Header
authHeader (AppAuth appAuth) =
    Http.header "authorization" appAuth
