module Main where

import Hlox qualified

main :: IO ()
main = do
  putStr "\nhlox> \r"
  prog <- getLine
  case prog of
    "quit" -> return ()
    "exit" -> return ()
    _ -> do
      print $ Hlox.exec prog
      main
