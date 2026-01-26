module Lexer.Helpers where

import Data.Either
import Data.List


fromFallibles :: [Either l r] -> Either [l] [r]
fromFallibles results
  | [] <- errs = Right (map (fromRight undefined) toks)
  | otherwise  = Left (map (fromLeft undefined) errs)
  where
    (errs, toks) = partition isLeft results
