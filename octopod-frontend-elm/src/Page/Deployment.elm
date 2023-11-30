module Page.Deployment exposing (..)

import Browser.Navigation as Nav
import Config exposing (Config, Settings)
import Html exposing (Html, div, text)
import Html.Common exposing (divClass)
import Page.Deployments exposing (Msg(..), pageView)


type alias Model =
    { settings : Settings
    , config : Config
    }


init : Settings -> Config -> ( Model, Cmd Msg )
init settings config =
    ( { settings = settings, config = config }, Cmd.none )


getNavKey : Model -> Nav.Key
getNavKey model =
    model.settings.navKey


getConfig : Model -> Config
getConfig model =
    model.config


getSettings : Model -> Settings
getSettings model =
    model.settings


type Msg
    = NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Deployments"
    , content = [ div [] [ text "lol" ] ]
    }
