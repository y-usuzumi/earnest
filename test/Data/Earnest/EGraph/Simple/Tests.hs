module Data.Earnest.EGraph.Simple.Tests where

import           Control.Arrow
import           Control.Monad
import           Data.Earnest.Currency
import           Data.Earnest.EGraph.Simple
import           Data.Earnest.TestData
import           Data.Earnest.TestData.TH
import           GHC.Generics
import           Test.Tasty
import           Test.Tasty.HUnit

testGraphFromExchanges :: TestTree
testGraphFromExchanges = testCase "graphFromExchanges" $ do
  xxis <- $(xxisFromNames [ [|Exchange1|]
                          , [|Exchange2|]
                          ])
  let g = graphFromExchanges xxis
  assertBool "Should support CNY" $ isCurrencySupported CNY g
  assertBool "Should support XRP" $ isCurrencySupported XRP g
  assertBool "Should not support BTS" $ not $ isCurrencySupported BTS g

  length (getTradableOptions BTC g) @?= 2
  length (getTradableOptions CNY g) @?= 1
  length (getTradableOptions XRP g) @?= 1
  length (getTradableOptions BTS g) @?= 0

testStronglyConnectedEGraphs :: TestTree
testStronglyConnectedEGraphs = testCase "stronglyConnectedEGraphs" $ do
  xxis <- $(xxisFromNames [ [|Exchange1|]
                          , [|Exchange2|]
                          , [|Exchange3|]
                          ])
  let g = graphFromExchanges xxis
  forM_ (stronglyConnectedEGraphs g) $ \scc -> do
    putStrLn "================"
    explain scc

testExplain :: TestTree
testExplain = testCase "explain" $ do
  xxis <- $(xxisFromNames [ [|Exchange1|]
                          , [|Exchange2|]
                          ])
  let g = graphFromExchanges xxis
  explain g

tests :: TestTree
tests = testGroup "Simple" [ testGraphFromExchanges
                           , testStronglyConnectedEGraphs
                           , testExplain
                           ]
