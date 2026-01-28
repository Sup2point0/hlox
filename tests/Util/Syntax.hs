module Util.Syntax where

import Test.Tasty
import Test.Tasty.HUnit


infix 1 ===
(===) :: (Show t, Eq t, HasCallStack) => t -> t -> Assertion
(===) = (@?=)

infix 1 !==
(!==) :: (Show t, Eq t, HasCallStack) => t -> t -> Assertion
(!==) l r = assertBool (show l ++ " /= " ++ show r) (l /= r)


testCollection :: String -> [Assertion] -> TestTree
testCollection name tests
  = testGroup name (zipWith (\n -> testCase ("#" ++ show n)) [1..] tests)
