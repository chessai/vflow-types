
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module VFlow.Types.NetFlow5
  ( NetFlow(..)
  , FlowsElt(..)
  , Header(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson(Value(..), FromJSON(..), ToJSON(..), pairs, (.:), (.=), object)
import Data.Monoid((<>))
import qualified GHC.Generics
import Net.Types (IPv4)

data FlowsElt = FlowsElt { 
    flowsEltDstAsNum :: Int,
    flowsEltStartTime :: Int,
    flowsEltL3Octets :: Int,
    flowsEltTCPFlags :: Int,
    flowsEltDstAddr :: IPv4,
    flowsEltTos :: Int,
    flowsEltPadding1 :: Int,
    flowsEltNextHop :: IPv4,
    flowsEltSrcPort :: Int,
    flowsEltInput :: Int,
    flowsEltPktCount :: Int,
    flowsEltOutput :: Int,
    flowsEltDstMask :: Int,
    flowsEltDstPort :: Int,
    flowsEltEndTime :: Int,
    flowsEltPadding2 :: Int,
    flowsEltSrcMask :: Int,
    flowsEltSrcAsNum :: Int,
    flowsEltSrcAddr :: IPv4,
    flowsEltProtType :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON FlowsElt where
  parseJSON (Object v) = FlowsElt <$> v .:  "DstAsNum" <*> v .:  "StartTime" <*> v .:  "L3Octets" <*> v .:  "TCPFlags" <*> v .:  "DstAddr" <*> v .:  "Tos" <*> v .:  "Padding1" <*> v .:  "NextHop" <*> v .:  "SrcPort" <*> v .:  "Input" <*> v .:  "PktCount" <*> v .:  "Output" <*> v .:  "DstMask" <*> v .:  "DstPort" <*> v .:  "EndTime" <*> v .:  "Padding2" <*> v .:  "SrcMask" <*> v .:  "SrcAsNum" <*> v .:  "SrcAddr" <*> v .:  "ProtType"
  parseJSON _          = mzero


instance ToJSON FlowsElt where
  toJSON     (FlowsElt {..}) = object ["DstAsNum" .= flowsEltDstAsNum, "StartTime" .= flowsEltStartTime, "L3Octets" .= flowsEltL3Octets, "TCPFlags" .= flowsEltTCPFlags, "DstAddr" .= flowsEltDstAddr, "Tos" .= flowsEltTos, "Padding1" .= flowsEltPadding1, "NextHop" .= flowsEltNextHop, "SrcPort" .= flowsEltSrcPort, "Input" .= flowsEltInput, "PktCount" .= flowsEltPktCount, "Output" .= flowsEltOutput, "DstMask" .= flowsEltDstMask, "DstPort" .= flowsEltDstPort, "EndTime" .= flowsEltEndTime, "Padding2" .= flowsEltPadding2, "SrcMask" .= flowsEltSrcMask, "SrcAsNum" .= flowsEltSrcAsNum, "SrcAddr" .= flowsEltSrcAddr, "ProtType" .= flowsEltProtType]
  toEncoding (FlowsElt {..}) = pairs  ("DstAsNum" .= flowsEltDstAsNum<>"StartTime" .= flowsEltStartTime<>"L3Octets" .= flowsEltL3Octets<>"TCPFlags" .= flowsEltTCPFlags<>"DstAddr" .= flowsEltDstAddr<>"Tos" .= flowsEltTos<>"Padding1" .= flowsEltPadding1<>"NextHop" .= flowsEltNextHop<>"SrcPort" .= flowsEltSrcPort<>"Input" .= flowsEltInput<>"PktCount" .= flowsEltPktCount<>"Output" .= flowsEltOutput<>"DstMask" .= flowsEltDstMask<>"DstPort" .= flowsEltDstPort<>"EndTime" .= flowsEltEndTime<>"Padding2" .= flowsEltPadding2<>"SrcMask" .= flowsEltSrcMask<>"SrcAsNum" .= flowsEltSrcAsNum<>"SrcAddr" .= flowsEltSrcAddr<>"ProtType" .= flowsEltProtType)


data Header = Header { 
    headerUNIXSecs :: Int,
    headerCount :: Int,
    headerEngType :: Int,
    headerVersion :: Int,
    headerSysUpTimeMSecs :: Int,
    headerUNIXNSecs :: Int,
    headerSmpInt :: Int,
    headerSeqNum :: Int,
    headerEngID :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Header where
  parseJSON (Object v) = Header <$> v .:  "UNIXSecs" <*> v .:  "Count" <*> v .:  "EngType" <*> v .:  "Version" <*> v .:  "SysUpTimeMSecs" <*> v .:  "UNIXNSecs" <*> v .:  "SmpInt" <*> v .:  "SeqNum" <*> v .:  "EngID"
  parseJSON _          = mzero


instance ToJSON Header where
  toJSON     (Header {..}) = object ["UNIXSecs" .= headerUNIXSecs, "Count" .= headerCount, "EngType" .= headerEngType, "Version" .= headerVersion, "SysUpTimeMSecs" .= headerSysUpTimeMSecs, "UNIXNSecs" .= headerUNIXNSecs, "SmpInt" .= headerSmpInt, "SeqNum" .= headerSeqNum, "EngID" .= headerEngID]
  toEncoding (Header {..}) = pairs  ("UNIXSecs" .= headerUNIXSecs<>"Count" .= headerCount<>"EngType" .= headerEngType<>"Version" .= headerVersion<>"SysUpTimeMSecs" .= headerSysUpTimeMSecs<>"UNIXNSecs" .= headerUNIXNSecs<>"SmpInt" .= headerSmpInt<>"SeqNum" .= headerSeqNum<>"EngID" .= headerEngID)


data NetFlow = NetFlow { 
    netflowAgentID :: IPv4,
    netflowHeader :: Header,
    netflowFlows :: [FlowsElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON NetFlow where
  parseJSON (Object v) = NetFlow <$> v .:  "AgentID" <*> v .:  "Header" <*> v .:  "Flows"
  parseJSON _          = mzero


instance ToJSON NetFlow where
  toJSON     (NetFlow {..}) = object ["AgentID" .= netflowAgentID, "Header" .= netflowHeader, "Flows" .= netflowFlows]
  toEncoding (NetFlow {..}) = pairs  ("AgentID" .= netflowAgentID<>"Header" .= netflowHeader<>"Flows" .= netflowFlows)

