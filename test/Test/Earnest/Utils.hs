module Test.Earnest.Utils where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Test.Earnest.Env
import           Test.Tasty
import           Test.Tasty.HUnit

testWithEnv :: TestName -> (TestEnv -> Assertion) -> TestTree
testWithEnv testName assertion = askOption $ \env ->
  testCase testName (assertion env)

-- TODO: MonadControlIO
testWithResource :: TestName
                 -> (TestEnv -> IO a)
                 -> (a -> Assertion)
                 -> (a -> IO ())
                 -> TestTree
testWithResource testName initializer assertion finalizer =
  testWithEnv testName $ \env -> do
  a <- initializer env
  assertion a `finally` finalizer a
