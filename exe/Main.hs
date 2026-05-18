module Main where

import System.IO

import Hlox qualified


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "\nhlox> "
  input <- getLine
  case input of
    "quit" -> return ()
    "exit" -> return ()
    "test" -> do
      src <- readFile "exe/prog.txt"
      print $ Hlox.exec src
    _ -> do
      print $ Hlox.exec input
      main
