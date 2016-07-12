import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import StringCalc

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList $ 
  [ TestLabel "Phase 1" $ TestList
    [ testCase "Empty is 0" $ 0 @=? calc "" 
    , testCase "1 is 1" $ 1 @=? calc "1"
    , testCase "List is summed" $ 3 @=? calc "1,2"
    ]
  , TestLabel "Phase 2" $ TestList
    [ testCase "List is summed" $ 45 @=? calc "1,2,3,4,5,6,7,8,9,0"
    ]
  , TestLabel "Phase 3" $ TestList
    [ testCase "Both commas and new lines" $ 6 @=? calc "1\n2,3"
    ]
  , TestLabel "Phase 4" $ TestList
    [ testCase "Variable delimiter" $ 3 @=? calc "//;\n1;2"
    ]
  ]
