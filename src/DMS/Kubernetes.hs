module DMS.Kubernetes
  ( helmPath
  , b2bHelmPath
  , kubectlPath
  , composeAppArgs
  , createInfraArgs
  , destroyAppArgs
  , destroyInfraArgs
  , deletePVCArgs
  , deleteCertArgs
  , CommandArg
  , ChartName(..)
  ) where

import Data.Coerce
import Data.String
import Data.Text as T
import System.Directory (findExecutable)

import Types


type CommandArg = Text
newtype ChartName = ChartName Text deriving (IsString)

helmPath :: IO (Maybe FilePath)
helmPath = findExecutable "helm"

b2bHelmPath :: IO (Maybe FilePath)
b2bHelmPath = findExecutable "b2b-helm"

kubectlPath :: IO (Maybe FilePath)
kubectlPath = findExecutable "kubectl"

appChartName, infraChartName :: ChartName
appChartName   = "b2b-dm-staging"
infraChartName = "b2b-infra-dm-staging"

namespace :: Text
namespace = "staging"

taggedRelease :: ChartName -> DeploymentTag -> Text
taggedRelease cn dt = coerce cn <> ":" <> coerce dt

releaseName :: ChartName -> DeploymentName -> Text
releaseName chartName dName = coerce chartName <> "-" <> coerce dName

composeAppArgs :: DeploymentName -> DeploymentTag -> EnvPairs -> [CommandArg]
composeAppArgs dName dTag envPairs = [
    "-d", "deploy"
  , "--release-name", releaseName appChartName dName
  , "--set", "b2b-app.email_domain=" <> coerce dName <> ".stage.thebestagent.pro"
  , "--set", "b2b-app.domain=" <> coerce dName <> ".stage.thebestagent.pro"
  , "--set", "b2b-app.connections.pg_instance=avia:avia@"
    <> rn <> "-postgres-0." <> rn <> "-postgres." <> namespace <> ":5432"
  , "--set", "b2b-app.connections.elastic=http://"
    <> rn <> "-elasticsearch." <> namespace <> ":9200"
  , "--set", "b2b-app.connections.elasic_hosts=http://"
    <> rn <> "-elasticsearch-0." <> rn <> "-elasticsearch."
    <> namespace <> ":9200"
  , "--set", "b2b-app.connections.redis="
    <> rn <> "-redis-0." <> rn <> "-redis." <> namespace <> ":6379"
  , "--set", "b2b-app.connections.kafka_ext="
    <> rn <> "-kafka-int-0." <> rn <> "-kafka-int:9092"
  , "--set", "b2b-app.connections.kafka_int="
    <> rn <> "-kafka-int-0." <> rn <> "-kafka-int:9092"
  , "--set", "global.staging_name=" <> coerce dName
  ]
  ++ mconcat (fmap (\a -> ["--set", a]) $ concatPairWithAppEnv <$> envPairs)
  ++ [taggedRelease appChartName dTag]
  where
    rn = releaseName infraChartName dName

createInfraArgs :: DeploymentName -> [Text]
createInfraArgs dName = [
    "-d", "deploy"
  , "--release-name", rn
  , "--set", "b2b-kafka-int.zk=" <> rn <> "-zk-0."
    <> rn <> "-zk." <> namespace <> "/int"
  , "--set", "b2b-elasticsearch.cluster_hosts="
    <> rn <> "-elasticsearch-0." <> rn <> "-elasticsearch." <> namespace
  , "--set", "b2b-postgres.postgres_db=" <> db
  , "--set", "global.staging_name=" <> coerce dName
  , "--set", "b2b-kibana.elasic_hosts=http://b2b-infra-dm-staging-" <> coerce dName
    <> "-elasticsearch-0.b2b-infra-dm-staging-" <> coerce dName
    <> "-elasticsearch.staging:9200"
  , "--set", "b2b-kibana.domain=kibana." <> coerce dName <> ".stage.thebestagent.pro"
  , coerce infraChartName
  ]
  where
    rn = releaseName infraChartName dName
    db = T.replace "-" "_" $ releaseName appChartName dName

composeDestroyArgs :: ChartName -> DeploymentName -> [CommandArg]
composeDestroyArgs chartName dName =
  ["delete", releaseName chartName dName, "--purge"]

destroyAppArgs, destroyInfraArgs :: DeploymentName -> [CommandArg]
destroyAppArgs   = composeDestroyArgs appChartName
destroyInfraArgs = composeDestroyArgs infraChartName

deletePVCArgs :: DeploymentName -> [CommandArg]
deletePVCArgs dName = [
      "delete"
    , "pvc"
    , "-n"
    , namespace
    , "-l"
    , "staging=" <> coerce dName
  ]

deleteCertArgs :: DeploymentName -> [CommandArg]
deleteCertArgs dName = [
      "delete"
    , "certificate"
    , "-n"
    , namespace
    , "b2b-dm-staging-" <> coerce dName <> "-tls"
    , "b2b-dm-staging-"<> coerce dName <> "-tls-tasker"
    , "b2b-infra-dm-staging-" <> coerce dName <> "-kibana-tls"
  ]
