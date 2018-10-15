module Data.Earnest.EGraph.FGL.Tests where

import           Control.Arrow
import           Control.Monad
import           Data.Earnest.Currency
import           Data.Earnest.EGraph.FGL
import           Data.Earnest.TestData
import           Data.Earnest.TestData.TH
import           GHC.Generics
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.HUnit

testGraphFromBourses :: TestTree
testGraphFromBourses = testCase "graphFromBourses" $ do
  let bs = $(boursesFromNames [ [|Bourse1|]
                              , [|Bourse2|]
                              ])
  g <- graphFromBourses bs
  assertBool "Should support CNY" $ isCurrencySupported CNY g
  assertBool "Should support XRP" $ isCurrencySupported XRP g
  assertBool "Should not support BTS" $ not $ isCurrencySupported BTS g

  length (getTradableOptions BTC g) @?= 2
  length (getTradableOptions CNY g) @?= 1
  length (getTradableOptions XRP g) @?= 1
  length (getTradableOptions BTS g) @?= 0

testExplain :: TestTree
testExplain = testCase "explain" $ do
  let bs = $(boursesFromNames [ [|Bourse1|]
                              , [|Bourse2|]
                              ])
  g <- graphFromBourses bs
  explain g

tests :: TestTree
tests = testGroup "FGL" [ testGraphFromBourses
                        , testExplain
                        ]
