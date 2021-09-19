module Main where

import System.Environment
import Tries

main :: IO ()
main = do
  args <- getArgs
  let filepath = args!!0
  let prefix = args!!1
  let k = read (args!!2) :: Int
  trie <- readTerms filepath
  let top_suggestions = autocomplete prefix trie k
  putStrLn $ show top_suggestions
  return ()
