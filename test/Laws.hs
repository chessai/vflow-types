{-# language TypeOperators #-}

module Main (main) where

import Data.Aeson.AutoType.Alternative
import Data.Proxy (Proxy(..))
import Net.Types (IPv4)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Vflow.Types.IpFix (IpFix(..))
import Vflow.Types.Netflow5 (Netflow5(..))
import Vflow.Types.Netflow9 (Netflow9(..))
import Vflow.Types.Sflow (Sflow(..))
import qualified Net.IPv4 as I4
import qualified Vflow.Types.IpFix as I
import qualified Vflow.Types.Netflow5 as N5
import qualified Vflow.Types.Netflow9 as N9
import qualified Vflow.Types.Sflow as S

main :: IO ()
main = lawsCheckMany laws

laws :: [(String,[Laws])]
laws =
  [ ("IPFIX", [jsonLaws (Proxy :: Proxy IpFix)])
  --, ("sflow", [jsonLaws (Proxy :: Proxy Sflow)])
  --, ("Netflow v5", [jsonLaws (Proxy :: Proxy Netflow5)])
  --, ("Netflow v9", [jsonLaws (Proxy :: Proxy Netflow9)])
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
  arbitrary = fmap I4.fromTupleOctets $ (,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary I.HexInt where
  arbitrary = I.HexInt <$> arbitrary

