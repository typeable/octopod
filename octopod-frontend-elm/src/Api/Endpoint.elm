module Api.Endpoint exposing
    ( Endpoint
    , appOverrideKeys
    , appOverrides
    , configJson
    , deleteDeployment
    , deploymentFullInfo
    , deploymentOverrideKeys
    , deploymentOverrides
    , deployments
    , projectName
    , request
    , restoreDeployment
    , saveDeployment
    )

import Api.Types.Deployment exposing (DeploymentName, unDeploymentName)
import Config exposing (AppUrl, unwrapAppUrl)
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


deploymentOverrides : AppUrl -> Endpoint
deploymentOverrides appUrl =
    url (Just appUrl) [ "api", "v1", "deployment_overrides" ] []


deploymentOverrideKeys : AppUrl -> Endpoint
deploymentOverrideKeys appUrl =
    url (Just appUrl) [ "api", "v1", "deployment_override_keys" ] []


appOverrides : AppUrl -> Endpoint
appOverrides appUrl =
    url (Just appUrl) [ "api", "v1", "application_overrides" ] []


appOverrideKeys : AppUrl -> Endpoint
appOverrideKeys appUrl =
    url (Just appUrl) [ "api", "v1", "application_override_keys" ] []


saveDeployment : AppUrl -> Endpoint
saveDeployment appUrl =
    url (Just appUrl) [ "api", "v1", "deployments" ] []


deploymentFullInfo : DeploymentName -> AppUrl -> Endpoint
deploymentFullInfo deploymentName appUrl =
    url (Just appUrl) [ "api", "v1", "deployments", unDeploymentName deploymentName, "full_info" ] []
