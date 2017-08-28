module Main where

import           Network.Haskoin.Constants
import qualified Network.Haskoin.Wallet.Spec
import qualified Network.Haskoin.Wallet.Tests (tests)
import qualified Network.Haskoin.Wallet.Units (tests)
import           Test.Framework               (defaultMain)
import           Test.Hspec                   (hspec)

main :: IO ()
main
    | networkName == "prodnet" = do
        hspec Network.Haskoin.Wallet.Spec.apiSpec
        defaultMain
            (  Network.Haskoin.Wallet.Tests.tests
            ++ Network.Haskoin.Wallet.Units.tests
            )
     | otherwise = error "Tests are only available on prodnet"

