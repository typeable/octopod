module DMS.AWS (findImageTag) where

import Control.Lens
import Data.Coerce
import Data.Text
import Network.AWS
import Network.AWS.ECR

import Types

repositoryName :: Text
repositoryName = "default"

findImageTag :: DeploymentTag -> IO (Maybe Text)
findImageTag dTag = do
  env <- newEnv Discover
  let
    lImages =
      listImages repositoryName
      & liFilter .~ Just (listImagesFilter & lifTagStatus .~ Just Tagged)
  resp <- runResourceT . runAWS env . send $ lImages
  handleResponse dTag env resp

handleResponse :: DeploymentTag -> Env -> ListImagesResponse -> IO (Maybe Text)
handleResponse dTag env resp = do
  let
    foundTags =
      resp
      ^. lirsImageIds
      ^.. each . iiImageTag . _Just . filtered (== coerce dTag)
  case foundTags of
    [foundTag] -> pure . Just $ foundTag
    _          -> findImageTagWithNextToken dTag env (resp ^. lirsNextToken)

findImageTagWithNextToken
  :: DeploymentTag
  -> Env -> Maybe Text
  -> IO (Maybe Text)
findImageTagWithNextToken _    _   Nothing      = pure Nothing
findImageTagWithNextToken dTag env (Just token) = do
  let
    lImages =
      listImages repositoryName
      & liFilter .~ Just (listImagesFilter & lifTagStatus .~ Just Tagged)
      & liNextToken .~ Just token
  resp <- runResourceT . runAWS env . send $ lImages
  handleResponse dTag env resp
