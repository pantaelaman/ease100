module Main where
import System.Environment

import qualified Text.Megaparsec as MP
import qualified Data.Text.IO as TIO

import Parser

main :: IO ()
main = do
  args <- getArgs
  inp <- TIO.readFile $ args !! 0
  case parse inp of
    Left err -> putStr $ MP.errorBundlePretty err
    Right bytecode -> print bytecode
