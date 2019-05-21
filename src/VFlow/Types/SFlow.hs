
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeOperators       #-}

module VFlow.Types.SFlow
  ( SFlow(..)
  , ExtRouter(..)
  , ExtSwitch(..)
  , L2(..)
  , L3(..)
  , L4(..)
  , RawHeader(..)
  , Records(..)
  , SamplesElt(..)
  ) where

import Control.Monad (mzero)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), pairs, (.:), (.=), object)
import Net.Types (IPv4, Mac)
import qualified GHC.Generics

data ExtRouter = ExtRouter { 
    extRouterNextHop :: IPv4,
    extRouterDstMask :: Int,
    extRouterSrcMask :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ExtRouter where
  parseJSON (Object v) = ExtRouter <$> v .:  "NextHop" <*> v .:  "DstMask" <*> v .:  "SrcMask"
  parseJSON _          = mzero


instance ToJSON ExtRouter where
  toJSON     (ExtRouter {..}) = object ["NextHop" .= extRouterNextHop, "DstMask" .= extRouterDstMask, "SrcMask" .= extRouterSrcMask]
  toEncoding (ExtRouter {..}) = pairs  ("NextHop" .= extRouterNextHop<>"DstMask" .= extRouterDstMask<>"SrcMask" .= extRouterSrcMask)


data ExtSwitch = ExtSwitch { 
    extSwitchDstPriority :: Int,
    extSwitchSrcVlan :: Int,
    extSwitchSrcPriority :: Int,
    extSwitchDstVlan :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ExtSwitch where
  parseJSON (Object v) = ExtSwitch <$> v .:  "DstPriority" <*> v .:  "SrcVlan" <*> v .:  "SrcPriority" <*> v .:  "DstVlan"
  parseJSON _          = mzero


instance ToJSON ExtSwitch where
  toJSON     (ExtSwitch {..}) = object ["DstPriority" .= extSwitchDstPriority, "SrcVlan" .= extSwitchSrcVlan, "SrcPriority" .= extSwitchSrcPriority, "DstVlan" .= extSwitchDstVlan]
  toEncoding (ExtSwitch {..}) = pairs  ("DstPriority" .= extSwitchDstPriority<>"SrcVlan" .= extSwitchSrcVlan<>"SrcPriority" .= extSwitchSrcPriority<>"DstVlan" .= extSwitchDstVlan)


data L2 = L2 { 
    l2Vlan :: Int,
    l2EtherType :: Int,
    l2DstMAC :: Mac,
    l2SrcMAC :: Mac
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON L2 where
  parseJSON (Object v) = L2 <$> v .:  "Vlan" <*> v .:  "EtherType" <*> v .:  "DstMAC" <*> v .:  "SrcMAC"
  parseJSON _          = mzero


instance ToJSON L2 where
  toJSON     (L2 {..}) = object ["Vlan" .= l2Vlan, "EtherType" .= l2EtherType, "DstMAC" .= l2DstMAC, "SrcMAC" .= l2SrcMAC]
  toEncoding (L2 {..}) = pairs  ("Vlan" .= l2Vlan<>"EtherType" .= l2EtherType<>"DstMAC" .= l2DstMAC<>"SrcMAC" .= l2SrcMAC)


data L3 = L3 { 
    l3TTL :: Int,
    l3Flags :: Int,
    l3TotalLen :: Int,
    l3Checksum :: Int,
    l3TOS :: Int,
    l3Dst :: IPv4,
    l3Protocol :: Int,
    l3Src :: IPv4,
    l3Version :: Int,
    l3ID :: Int,
    l3FragOff :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON L3 where
  parseJSON (Object v) = L3 <$> v .:  "TTL" <*> v .:  "Flags" <*> v .:  "TotalLen" <*> v .:  "Checksum" <*> v .:  "TOS" <*> v .:  "Dst" <*> v .:  "Protocol" <*> v .:  "Src" <*> v .:  "Version" <*> v .:  "ID" <*> v .:  "FragOff"
  parseJSON _          = mzero


instance ToJSON L3 where
  toJSON     (L3 {..}) = object ["TTL" .= l3TTL, "Flags" .= l3Flags, "TotalLen" .= l3TotalLen, "Checksum" .= l3Checksum, "TOS" .= l3TOS, "Dst" .= l3Dst, "Protocol" .= l3Protocol, "Src" .= l3Src, "Version" .= l3Version, "ID" .= l3ID, "FragOff" .= l3FragOff]
  toEncoding (L3 {..}) = pairs  ("TTL" .= l3TTL<>"Flags" .= l3Flags<>"TotalLen" .= l3TotalLen<>"Checksum" .= l3Checksum<>"TOS" .= l3TOS<>"Dst" .= l3Dst<>"Protocol" .= l3Protocol<>"Src" .= l3Src<>"Version" .= l3Version<>"ID" .= l3ID<>"FragOff" .= l3FragOff)


data L4 = L4 { 
    l4Flags :: Int,
    l4DataOffset :: Int,
    l4SrcPort :: Int,
    l4Reserved :: Int,
    l4DstPort :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON L4 where
  parseJSON (Object v) = L4 <$> v .:  "Flags" <*> v .:  "DataOffset" <*> v .:  "SrcPort" <*> v .:  "Reserved" <*> v .:  "DstPort"
  parseJSON _          = mzero


instance ToJSON L4 where
  toJSON     (L4 {..}) = object ["Flags" .= l4Flags, "DataOffset" .= l4DataOffset, "SrcPort" .= l4SrcPort, "Reserved" .= l4Reserved, "DstPort" .= l4DstPort]
  toEncoding (L4 {..}) = pairs  ("Flags" .= l4Flags<>"DataOffset" .= l4DataOffset<>"SrcPort" .= l4SrcPort<>"Reserved" .= l4Reserved<>"DstPort" .= l4DstPort)


data RawHeader = RawHeader { 
    rawHeaderL2 :: L2,
    rawHeaderL3 :: L3,
    rawHeaderL4 :: L4
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON RawHeader where
  parseJSON (Object v) = RawHeader <$> v .:  "L2" <*> v .:  "L3" <*> v .:  "L4"
  parseJSON _          = mzero


instance ToJSON RawHeader where
  toJSON     (RawHeader {..}) = object ["L2" .= rawHeaderL2, "L3" .= rawHeaderL3, "L4" .= rawHeaderL4]
  toEncoding (RawHeader {..}) = pairs  ("L2" .= rawHeaderL2<>"L3" .= rawHeaderL3<>"L4" .= rawHeaderL4)


data Records = Records { 
    recordsExtRouter :: ExtRouter,
    recordsExtSwitch :: ExtSwitch,
    recordsRawHeader :: RawHeader
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Records where
  parseJSON (Object v) = Records <$> v .:  "ExtRouter" <*> v .:  "ExtSwitch" <*> v .:  "RawHeader"
  parseJSON _          = mzero


instance ToJSON Records where
  toJSON     (Records {..}) = object ["ExtRouter" .= recordsExtRouter, "ExtSwitch" .= recordsExtSwitch, "RawHeader" .= recordsRawHeader]
  toEncoding (Records {..}) = pairs  ("ExtRouter" .= recordsExtRouter<>"ExtSwitch" .= recordsExtSwitch<>"RawHeader" .= recordsRawHeader)


data SamplesElt = SamplesElt { 
    samplesEltDrops :: Int,
    samplesEltSourceID :: Int,
    samplesEltRecords :: Records,
    samplesEltInput :: Int,
    samplesEltSequenceNo :: Int,
    samplesEltSamplingRate :: Int,
    samplesEltOutput :: Int,
    samplesEltRecordsNo :: Int,
    samplesEltSamplePool :: Int
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SamplesElt where
  parseJSON (Object v) = SamplesElt <$> v .:  "Drops" <*> v .:  "SourceID" <*> v .:  "Records" <*> v .:  "Input" <*> v .:  "SequenceNo" <*> v .:  "SamplingRate" <*> v .:  "Output" <*> v .:  "RecordsNo" <*> v .:  "SamplePool"
  parseJSON _          = mzero


instance ToJSON SamplesElt where
  toJSON     (SamplesElt {..}) = object ["Drops" .= samplesEltDrops, "SourceID" .= samplesEltSourceID, "Records" .= samplesEltRecords, "Input" .= samplesEltInput, "SequenceNo" .= samplesEltSequenceNo, "SamplingRate" .= samplesEltSamplingRate, "Output" .= samplesEltOutput, "RecordsNo" .= samplesEltRecordsNo, "SamplePool" .= samplesEltSamplePool]
  toEncoding (SamplesElt {..}) = pairs  ("Drops" .= samplesEltDrops<>"SourceID" .= samplesEltSourceID<>"Records" .= samplesEltRecords<>"Input" .= samplesEltInput<>"SequenceNo" .= samplesEltSequenceNo<>"SamplingRate" .= samplesEltSamplingRate<>"Output" .= samplesEltOutput<>"RecordsNo" .= samplesEltRecordsNo<>"SamplePool" .= samplesEltSamplePool)


data SFlow = SFlow { 
    sflowIPAddress :: IPv4,
    sflowAgentSubID :: Int,
    sflowIPVersion :: Int,
    sflowSequenceNo :: Int,
    sflowSysUpTime :: Int,
    sflowSamplesNo :: Int,
    sflowVersion :: Int,
    sflowSamples :: [SamplesElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SFlow where
  parseJSON (Object v) = SFlow <$> v .:  "IPAddress" <*> v .:  "AgentSubID" <*> v .:  "IPVersion" <*> v .:  "SequenceNo" <*> v .:  "SysUpTime" <*> v .:  "SamplesNo" <*> v .:  "Version" <*> v .:  "Samples"
  parseJSON _          = mzero


instance ToJSON SFlow where
  toJSON     (SFlow {..}) = object ["IPAddress" .= sflowIPAddress, "AgentSubID" .= sflowAgentSubID, "IPVersion" .= sflowIPVersion, "SequenceNo" .= sflowSequenceNo, "SysUpTime" .= sflowSysUpTime, "SamplesNo" .= sflowSamplesNo, "Version" .= sflowVersion, "Samples" .= sflowSamples]
  toEncoding (SFlow {..}) = pairs  ("IPAddress" .= sflowIPAddress<>"AgentSubID" .= sflowAgentSubID<>"IPVersion" .= sflowIPVersion<>"SequenceNo" .= sflowSequenceNo<>"SysUpTime" .= sflowSysUpTime<>"SamplesNo" .= sflowSamplesNo<>"Version" .= sflowVersion<>"Samples" .= sflowSamples)

