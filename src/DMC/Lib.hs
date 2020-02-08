{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DMC.Lib
    ( runDMC
    ) where

import Options.Generic

data Args
  = Create { name :: Text, template :: Text, envs :: [Text] }
  | List
  | Edit { name :: Text }
  | Destroy { name :: Text }
  | Update { name :: Text }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers $ defaultModifiers { shortNameModifier = firstLetter }

runDMC :: IO ()
runDMC = do
  args <- getRecord "DMC"
  print (args :: Args)
