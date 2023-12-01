module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Api.Types.Deployment exposing (DeploymentName(..), unDeploymentName)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- ROUTING


type Route
    = Deployments
    | Deployment DeploymentName


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Deployments (s "deployments")
        , Parser.map Deployment (s "deployments" </> Parser.custom "deployment" (\str -> Just (DeploymentName str)))
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    "#/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Deployments ->
            [ "deployments" ]

        Deployment deployment ->
            [ "deployments", unDeploymentName deployment ]
