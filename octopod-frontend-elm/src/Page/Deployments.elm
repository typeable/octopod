module Page.Deployments exposing (..)

import Browser.Navigation as Nav
import Http
import Html exposing (..)
import Deployments exposing (Deployment)
import Deployments exposing (deploymentsDecoder)
import Deployments exposing (Deployments)
import String exposing (fromInt)

type Model
  = Model Nav.Key Deployments
  | DeploymentsLoading Nav.Key
  | FatalError Nav.Key


init : Nav.Key -> ( Model, Cmd Msg )
init key =
  ( DeploymentsLoading key
  , getConfig )

getNavKey : Model -> Nav.Key
getNavKey model =
  case model of
    Model key _ -> key
    DeploymentsLoading key -> key
    FatalError key -> key


type Msg
  = DeploymentsResponse (Result Http.Error (List Deployment))

getConfig : Cmd Msg
getConfig =
  Http.get
    { url = "http://localhost:3100/deployments"
    , expect = Http.expectJson DeploymentsResponse deploymentsDecoder
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case (msg, model) of
    (DeploymentsResponse result, DeploymentsLoading key)  ->
      case result of
        Ok deployments ->
          (Model key deployments, Cmd.none)
        Err _ ->
          (FatalError key, Cmd.none)
    (_, _) -> (FatalError (getNavKey model), Cmd.none)

view : Model -> Html Msg
view model =
  case model of
    Model _ deployments ->
      div []
        [ h2 [] [ text "Got it!" ]
        , text (fromInt (List.length deployments))
        ]
    DeploymentsLoading _ -> div [] [text "loading deployments..."]
    FatalError _ -> div [] [text "fatal error..."]
