module Earnest.Reactor.Tests where

import           Control.Lens
import           Control.Monad.State
import           Data.Earnest.Action
import           Earnest.Reactor
import           Streamly            as S
import qualified Streamly.Prelude    as S
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.HUnit

testReactor :: TestEnv -> TestTree
testReactor env = testCase "reactor" $ do
  let zeroReactor = Reactor { _exchanges = []
                            , _loop = 0
                            }
  s <- execStateT (S.runStream (runReactor :: SerialT (StateT Reactor IO) (Ron SerialT Identity))) zeroReactor
  (s ^. loop) @?= 5
  rons <- evalStateT (S.toList $ S.serially runReactor) zeroReactor
  let actionCount = length $ join $ map (\(Ron action) -> return $ S.toList action) rons
  actionCount @?= 5

tests :: TestEnv -> TestTree
tests env = testGroup "Transaction" [ testReactor env
                                    ]
