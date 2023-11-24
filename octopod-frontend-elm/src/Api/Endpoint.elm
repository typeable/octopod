module Api.Endpoint exposing (Endpoint, configJson, deleteDeployment, deployments, projectName, request, restoreDeployment)

import Config exposing (AppUrl, unwrapAppUrl)
import Deployments exposing (DeploymentName, unDeploymentName)
import Http
import Maybe exposing (map, withDefault)
import Url.Builder exposing (QueryParameter)


request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , tracker = config.tracker
        }


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : Maybe AppUrl -> List String -> List QueryParameter -> Endpoint
url mAppUrl paths queryParams =
    Url.Builder.crossOrigin (withDefault "http://localhost:3100" (map unwrapAppUrl mAppUrl))
        paths
        queryParams
        |> Endpoint


configJson : Endpoint
configJson =
    url Nothing [ "config.json" ] []


deployments : AppUrl -> Endpoint
deployments appUrl =
    url (Just appUrl) [ "api", "v1", "deployments" ] []


projectName : AppUrl -> Endpoint
projectName appUrl =
    url (Just appUrl) [ "api", "v1", "project_name" ] []


deleteDeployment : DeploymentName -> AppUrl -> Endpoint
deleteDeployment deploymentName appUrl =
    url (Just appUrl) [ "api", "v1", "deployments", unDeploymentName deploymentName ] []


restoreDeployment : DeploymentName -> AppUrl -> Endpoint
restoreDeployment deploymentName appUrl =
    url (Just appUrl) [ "api", "v1", "deployments", unDeploymentName deploymentName, "restore" ] []
