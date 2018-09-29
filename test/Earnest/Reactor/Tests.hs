module Earnest.Reactor.Tests where

import           Control.Lens
import           Control.Monad.State
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
  let lengths = map (^. i) $ join $ map (^. actions) rons
  lengths @?= [0, 0, 0, 0, 0]

tests :: TestTree
tests = testGroup "Transaction" [ testReactor
                                ]
