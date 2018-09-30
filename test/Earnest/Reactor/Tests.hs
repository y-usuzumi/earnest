module Earnest.Reactor.Tests where

import           Control.Lens
import           Control.Monad.State
import           Data.Earnest.Action
import           Earnest.Reactor
import qualified Streamly            as S
import qualified Streamly.Prelude    as S
import           Test.Tasty
import           Test.Tasty.HUnit

testReactor :: TestTree
testReactor = testCase "reactor" $ do
  let zeroReactor = Reactor { _exchanges = []
                            , _loop = 0
                            }
  s <- execStateT (S.runStream $ S.serially runReactor) zeroReactor
  (s ^. loop) @?= 5
  rons <- evalStateT (S.toList $ S.serially runReactor) zeroReactor
  let actionCount = length $ join $ map (^. actions) rons
  actionCount @?= 5

tests :: TestTree
tests = testGroup "Transaction" [ testReactor
                                ]
