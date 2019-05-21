
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module JsonEmptyKey where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics

data DataSetsEltElt = DataSetsEltElt { 
    dataSetsEltEltV :: Text:|:Int:|:[(Maybe Value)],
    dataSetsEltEltI :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON DataSetsEltElt where
  parseJSON (Object v) = DataSetsEltElt <$> v .:  "V" <*> v .:  "I"
  parseJSON _          = mzero


instance ToJSON DataSetsEltElt where
  toJSON     (DataSetsEltElt {..}) = object ["V" .= dataSetsEltEltV, "I" .= dataSetsEltEltI]
  toEncoding (DataSetsEltElt {..}) = pairs  ("V" .= dataSetsEltEltV<>"I" .= dataSetsEltEltI)


data Header = Header { 
    headerLength :: Int,
    headerSequenceNo :: Int,
    headerExportTime :: Int,
    headerVersion :: Int,
    headerDomainID :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Header where
  parseJSON (Object v) = Header <$> v .:  "Length" <*> v .:  "SequenceNo" <*> v .:  "ExportTime" <*> v .:  "Version" <*> v .:  "DomainID"
  parseJSON _          = mzero


instance ToJSON Header where
  toJSON     (Header {..}) = object ["Length" .= headerLength, "SequenceNo" .= headerSequenceNo, "ExportTime" .= headerExportTime, "Version" .= headerVersion, "DomainID" .= headerDomainID]
  toEncoding (Header {..}) = pairs  ("Length" .= headerLength<>"SequenceNo" .= headerSequenceNo<>"ExportTime" .= headerExportTime<>"Version" .= headerVersion<>"DomainID" .= headerDomainID)


data TopLevel = TopLevel { 
    topLevelAgentID :: Text,
    topLevelHeader :: Header,
    topLevelDataSets :: [[DataSetsEltElt]]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "AgentID" <*> v .:  "Header" <*> v .:  "DataSets"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["AgentID" .= topLevelAgentID, "Header" .= topLevelHeader, "DataSets" .= topLevelDataSets]
  toEncoding (TopLevel {..}) = pairs  ("AgentID" .= topLevelAgentID<>"Header" .= topLevelHeader<>"DataSets" .= topLevelDataSets)




parse :: FilePath -> IO TopLevel
parse filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left  err -> fatal $ case (eitherDecode input :: Either String Value) of
                           Left  err -> "Invalid JSON file: " ++ filename ++ " ++ err"
                           Right _   -> "Mismatched JSON value from file: " ++ filename
                                     ++ "\n" ++ err
      Right r   -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess

Running Haskell module: ["runghc"]["","json-samples/ipfix.json"]
