module Main where
import System.Environment

import Data.List
import Control.Lens
import qualified Text.Megaparsec as MP

import Parser

main :: IO ()
main = do
  args <- getArgs
  (parseResult, st) <- parse $ args !! 0
  mconcat $ intersperse (putStr "\n") $ (putStr . MP.errorBundlePretty) <$> st^.pstErrs
  putStr "\n"
  case parseResult of
    Left err -> putStr $ MP.errorBundlePretty err
    Right bytecode -> print bytecode
