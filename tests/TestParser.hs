module Main(main) where

import Test.HUnit
import RLang.Parser
import RLang.L1DataTypes
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text
import System.IO.Unsafe
import System.Exit

readTestCaseFile :: String -> BS.ByteString
readTestCaseFile filepath = BS.pack $ unsafePerformIO $ readFile filepath

----------------- Triv Tests ------------------

trivOpTest :: Triv -> String -> Test
trivOpTest expected filepath = TestCase $ assertEqual "" 
  (Just $ Prog $ TrivOp expected)
  (parse $ readTestCaseFile filepath)

five = ConstantOp $ N 5
asConstant s = ConstantOp $ S $ pack s
 
notOpTest      = trivOpTest (Not five)                     "tests/json/not-test.json"
resultOpTest   = trivOpTest (ResultOf $ asConstant "some-tagger")   "tests/json/resultOf-test.json"
valueOfOpTest  = trivOpTest (ValueOf  $ asConstant "some-variable") "tests/json/valueOf-test.json"
tableRefOpTest = trivOpTest (TableRef $ asConstant "some-table")    "tests/json/tableRef-test.json"
eqOpTest       = trivOpTest (Eq  five five) "tests/json/eq-test.json"
orOpTest       = trivOpTest (Or  five five) "tests/json/or-test.json"
andOpTest      = trivOpTest (And five five) "tests/json/and-test.json"
neqOpTest      = trivOpTest (Neq five five) "tests/json/neq-test.json"
addOpTest      = trivOpTest (Add five five) "tests/json/add-test.json"
mulOpTest      = trivOpTest (Mul five five) "tests/json/mul-test.json"
divOpTest      = trivOpTest (Div five five) "tests/json/div-test.json"
modOpTest      = trivOpTest (Mod five five) "tests/json/mod-test.json"
grtOpTest      = trivOpTest (Grt five five) "tests/json/grt-test.json"
lstOpTest      = trivOpTest (Lst five five) "tests/json/lst-test.json"
geqOpTest      = trivOpTest (Geq five five) "tests/json/geq-test.json"
leqOpTest      = trivOpTest (Leq five five) "tests/json/leq-test.json"
smallTest      = trivOpTest (Leq five five) "tests/json/small-test.json"

trivOpTests = TestList [notOpTest, resultOpTest, valueOfOpTest, tableRefOpTest, eqOpTest,
  orOpTest, andOpTest, neqOpTest, addOpTest, mulOpTest, divOpTest, modOpTest, grtOpTest, lstOpTest, geqOpTest, leqOpTest, smallTest]

----------------- PrimOp Tests ------------------
  
primOpTest :: Prim -> String -> Test
primOpTest expected filepath = TestCase $ assertEqual "" 
  (Just $ Prog $ PrimOp expected)
  (parse $ readTestCaseFile filepath)

epochSecondsOpTest = primOpTest NowEpochSeconds                  "tests/json/epochSeconds-test.json"
randomIntOpTest    = primOpTest (RandomInt five)                 "tests/json/randomInt-test.json"
timestampDiffTest  = primOpTest (EpochSecondsFromTimestamp five) "tests/json/epochSecondsFromTimestamp-test.json"

primOpTests = TestList [epochSecondsOpTest, randomIntOpTest, timestampDiffTest]

main :: IO ()
main = do 
  testResults <- runTestTT $ TestList [trivOpTests, primOpTests]
  if 0 /= (failures testResults) || 0 /= (errors testResults) then exitFailure else return ()

