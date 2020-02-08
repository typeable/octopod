{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DMS.Lib
    ( runDMS
    ) where

import Options.Generic

data Args
  = Args { port :: Int, db :: Text, db_pool_size :: Int }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers $ defaultModifiers { fieldNameModifier = fmap (\c -> if c == '_' then '-' else c) }

runDMS :: IO ()
runDMS = do
  args <- getRecord "DMS"
  print (args :: Args)
