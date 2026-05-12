module Main where

main :: IO ()
main = do
  prog <- readLine
  case evalProgram (parse prog) of
    Left  e -> print e
    Right r -> print r
