
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module VFlow.Types.NetFlow9
  ( NetFlow(..)
  , Header(..)
  , DataSetsEltElt(..)
  , HexInt(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson(Value(..), FromJSON(..), ToJSON(..), pairs, (.:), (.=), object)
import Data.Aeson.AutoType.Alternative
import Data.Int (Int64)
import Data.Monoid((<>))
import Net.Types (IPv4)
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
    headerUNIXSecs :: Int,
    headerSrcID :: Int,
    headerCount :: Int,
    headerSysUpTime :: Int,
    headerVersion :: Int,
    headerSeqNum :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Header where
  parseJSON (Object v) = Header <$> v .:  "UNIXSecs" <*> v .:  "SrcID" <*> v .:  "Count" <*> v .:  "SysUpTime" <*> v .:  "Version" <*> v .:  "SeqNum"
  parseJSON _          = mzero


instance ToJSON Header where
  toJSON     (Header {..}) = object ["UNIXSecs" .= headerUNIXSecs, "SrcID" .= headerSrcID, "Count" .= headerCount, "SysUpTime" .= headerSysUpTime, "Version" .= headerVersion, "SeqNum" .= headerSeqNum]
  toEncoding (Header {..}) = pairs  ("UNIXSecs" .= headerUNIXSecs<>"SrcID" .= headerSrcID<>"Count" .= headerCount<>"SysUpTime" .= headerSysUpTime<>"Version" .= headerVersion<>"SeqNum" .= headerSeqNum)


data NetFlow = NetFlow { 
    netflowAgentID :: IPv4,
    netflowHeader :: Header,
    netflowDataSets :: [[DataSetsEltElt]]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON NetFlow where
  parseJSON (Object v) = NetFlow <$> v .:  "AgentID" <*> v .:  "Header" <*> v .:  "DataSets"
  parseJSON _          = mzero


instance ToJSON NetFlow where
  toJSON     (NetFlow {..}) = object ["AgentID" .= netflowAgentID, "Header" .= netflowHeader, "DataSets" .= netflowDataSets]
  toEncoding (NetFlow {..}) = pairs  ("AgentID" .= netflowAgentID<>"Header" .= netflowHeader<>"DataSets" .= netflowDataSets)
