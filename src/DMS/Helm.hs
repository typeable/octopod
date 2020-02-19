{-# LANGUAGE OverloadedStrings #-}

module DMS.Helm
  ( helmPath
  , b2bHelmPath
  , createAppArgs
  , createInfraArgs
  , editAppArgs
  , updateAppArgs
  , destroyAppArgs
  , destroyInfraArgs
  ) where

import System.Directory (findExecutable)

helmPath :: IO (Maybe FilePath)
helmPath = findExecutable "helm"

b2bHelmPath :: IO (Maybe FilePath)
b2bHelmPath = findExecutable "b2b-helm"

appChartName = "b2b-dm-staging"
infraChartName = "b2b-infra-dm-staging"
namespace = "staging"

releaseName chartName name = chartName ++ "-" ++ namespace ++ "-" ++ name

createAppArgs name tag envs = [
    "-d", "deploy"
  , "--release-name", releaseName appChartName name
  , "--set", "'b2b-app.email_domain=" ++ name ++ ".staging.thebestagent.pro'"
  , "--set", "'b2b-app.domain=" ++ name ++ ".staging.thebestagent.pro'"
  , "--set", "'b2b-app.connections.pg_instance=avia:avia@" ++ rn ++ "-postgres-0." ++ rn ++ "-postgres." ++ namespace ++ ":5432'"
  , "--set", "'b2b-app.connections.elastic=http://" ++ rn ++ "-elasticsearch." ++ namespace ++ ":9200'"
  , "--set", "'b2b-app.connections.elasic_hosts=http://" ++ rn ++ "-elasticsearch-0." ++ rn ++ "-elasticsearch." ++ namespace ++ ":9200'"
  , "--set", "'b2b-app.connections.redis=" ++ rn ++ "-redis-0." ++ rn ++ "-redis." ++ namespace ++ ":6379'"
  , "--set", "'b2b-app.connections.kafka_ext=" ++ rn ++ "-kafka-int-0." ++ rn ++ "-kafka-int:9092'"
  , "--set", "'b2b-app.connections.kafka_int=" ++ rn ++ "-kafka-int-0." ++ rn ++ "-kafka-int:9092'"
  ]
  ++ mconcat (fmap ((\a b -> [a, b]) "--set") envs)
  ++ [
    appChartName ++ ":" ++ tag
  ]
  where rn = releaseName infraChartName name

createInfraArgs name = [
    "-d", "deploy"
  , "--release-name", rn
  , "--set", "'b2b-kafka-int.zk=" ++ rn ++ "-zk-0." ++ rn ++ "-zk." ++ namespace ++ "/int'"
  , "--set", "'b2b-elasticsearch.cluster_hosts=" ++ rn ++ "-elasticsearch-0." ++ rn ++ "-elasticsearch." ++ namespace ++ "'"
  , "--set", "'b2b-postgres.postgres_db=avia'"
  , infraChartName
  ]
  where rn = releaseName infraChartName name

editAppArgs = createAppArgs

updateAppArgs = createAppArgs

destroyAppArgs name = [releaseName appChartName name, "--purge"]

destroyInfraArgs name = [releaseName infraChartName name, "--purge"]
