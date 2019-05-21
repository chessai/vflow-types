
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeOperators       #-}

module Vflow.Types.IpFix
  ( IpFix(..)
  , DataSetsEltElt(..)
  , Header(..)
  , HexInt(..)
  )
  where

import Control.Monad (mzero)
import Data.Aeson(Value(..), FromJSON(..), ToJSON(..), pairs, (.:), (.=), object)
import Data.Aeson.AutoType.Alternative
import Data.Int (Int64)
import Data.Monoid((<>))
import Net.IPv4 (IPv4)
import Text.Read (readMaybe)
import qualified Data.Scientific as Sci
import qualified Data.Text
import qualified GHC.Generics

newtype HexInt = HexInt Int64
  deriving (Eq, Show)

instance FromJSON HexInt where
  parseJSON (String s) = case readMaybe (Data.Text.unpack s) of
    Nothing -> mzero
    Just x -> pure (HexInt x)
  parseJSON (Number s) = if Sci.isInteger s
    then case Sci.toBoundedInteger s of
      Nothing -> mzero
      Just x -> pure (HexInt x)
    else mzero
  parseJSON _ = mzero

instance ToJSON HexInt where
  toJSON (HexInt x) = toJSON x
  toEncoding (HexInt x) = toEncoding x

data DataSetsEltElt = DataSetsEltElt { 
    dataSetsEltEltV :: IPv4 :|: HexInt,
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


data IpFix = IpFix { 
    ipFixAgentID :: IPv4,
    ipFixHeader :: Header,
    ipFixDataSets :: ~[[DataSetsEltElt]]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON IpFix where
  parseJSON (Object v) = IpFix <$> v .:  "AgentID" <*> v .:  "Header" <*> v .:  "DataSets"
  parseJSON _          = mzero


instance ToJSON IpFix where
  toJSON     (IpFix {..}) = object ["AgentID" .= ipFixAgentID, "Header" .= ipFixHeader, "DataSets" .= ipFixDataSets]
  toEncoding (IpFix {..}) = pairs  ("AgentID" .= ipFixAgentID<>"Header" .= ipFixHeader<>"DataSets" .= ipFixDataSets)
