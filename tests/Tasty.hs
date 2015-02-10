module Main where

import Test.Tasty
--import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import System.Exit (exitFailure)

main :: IO ()
main = do
    defaultMain $ testGroup "Tests"
        [ testGroup "QuickCheck"
            [ QC.testProperty "id == reverse . reverse" $
                \ (list :: [Int]) -> list == reverse (reverse list)
            ]
        ]
