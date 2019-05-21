{-# language TypeOperators #-}

module Main (main) where

import Data.Aeson.AutoType.Alternative
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Net.Types (IPv4,Mac)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Vflow.Types.IpFix (IpFix(..))
import Vflow.Types.Netflow5 (Netflow5(..))
import Vflow.Types.Netflow9 (Netflow9(..))
import Vflow.Types.Sflow (Sflow(..))
import qualified Net.IPv4 as I4
import qualified Net.Mac as Mac
import qualified Vflow.Types.IpFix as I
import qualified Vflow.Types.Netflow5 as N5
import qualified Vflow.Types.Netflow9 as N9
import qualified Vflow.Types.Sflow as S

main :: IO ()
main = lawsCheckMany laws

laws :: [(String,[Laws])]
laws =
  [ ("IPFIX", [jsonLaws (Proxy :: Proxy IpFix)])
  , ("sflow", [jsonLaws (Proxy :: Proxy Sflow)])
  , ("Netflow v5", [jsonLaws (Proxy :: Proxy Netflow5)])
  , ("Netflow v9", [jsonLaws (Proxy :: Proxy Netflow9)])
  ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :|: b) where
  arbitrary = frequency [ (1, AltLeft <$> arbitrary), (1, AltRight <$> arbitrary) ]

instance Arbitrary IpFix where
  arbitrary = IpFix <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary I.Header where
  arbitrary = I.Header <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary I.DataSetsEltElt where
  arbitrary = I.DataSetsEltElt <$> arbitrary <*> arbitrary

instance Arbitrary IPv4 where
  arbitrary = I4.ipv4 <$> word8 <*> word8 <*> word8 <*> word8

instance Arbitrary Mac where
  arbitrary = Mac.fromOctets <$> word8 <*> word8 <*> word8 <*> word8 <*> word8 <*> word8
 
instance Arbitrary I.HexInt where
  arbitrary = I.HexInt <$> arbitrary

instance Arbitrary S.ExtRouter where
  arbitrary = S.ExtRouter <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.ExtSwitch where
  arbitrary = S.ExtSwitch <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.L2 where
  arbitrary = S.L2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.L3 where
  arbitrary = S.L3 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.L4 where
  arbitrary = S.L4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.RawHeader where
  arbitrary = S.RawHeader <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.Records where
  arbitrary = S.Records <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.SamplesElt where
  arbitrary = S.SamplesElt <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary S.Sflow where
  arbitrary = S.Sflow <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

word8 :: Gen Word8
word8 = arbitrary `suchThat` (\x -> x >= 0 && x <= 255)

instance Arbitrary N5.FlowsElt where
  arbitrary = N5.FlowsElt <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary N5.Header where
  arbitrary = N5.Header <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary N5.Netflow5 where
  arbitrary = N5.Netflow5 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary N9.HexInt where
  arbitrary = N9.HexInt <$> arbitrary

instance Arbitrary N9.DataSetsEltElt where
  arbitrary = N9.DataSetsEltElt <$> arbitrary <*> arbitrary

instance Arbitrary N9.Header where
  arbitrary = N9.Header <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary N9.Netflow9 where
  arbitrary = N9.Netflow9 <$> arbitrary <*> arbitrary <*> arbitrary
