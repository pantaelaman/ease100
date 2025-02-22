module Main where

import qualified Data.HashMap.Strict as HM
import Numeric
import Data.Int
import Unsafe.Coerce
import Data.Word
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Control.Applicative
import qualified Options.Applicative as O
import System.IO
import Control.Lens
import qualified Text.Megaparsec as MP
import Control.Monad.Cont

import Parser

data Args = Args
  { fname :: String
  , mifname :: Maybe String
  , lblname :: Maybe String
  , crossname :: Maybe String }

argParser :: O.Parser Args
argParser = Args
  <$> O.strArgument (O.metavar "FILE")
  <*> optional (O.strOption
    (  O.metavar "MIF FILE"
    <> O.short 'm'
    <> O.long "mif"))
  <*> optional (O.strOption
    (  O.metavar "LABELS FILE"
    <> O.short 'l'
    <> O.long "labels"))
  <*> optional (O.strOption
    (  O.metavar "E FILE"
    <> O.short 'e'
    <> O.long "cross"))

argInfo :: O.ParserInfo Args
argInfo = O.info (argParser <**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Compiles FILE, generating a .mif and .labels file; also optionally a .e file"
  <> O.header "ease - a compiler for extended ase100 programs")

main :: IO ()
main = do
  args <- O.execParser argInfo
  -- drops until reaching the last period
  let sname = reverse . dropWhile (/= '.') . reverse $ fname args
  let mifn = fromMaybe (sname ++ "mif") $ mifname args
  let lbln = fromMaybe (sname ++ "labels") $ lblname args
  (parseResult, st) <- parse $ fname args
  mconcat $ intersperse (putStr "\n") $ (putStr . MP.errorBundlePretty) <$> st^.pstErrs
  putStr "\n"
  case parseResult of
    Left err -> putStr $ MP.errorBundlePretty err
    Right bytecode -> flip runContT return $ do
      mifFile <- ContT $ withFile mifn WriteMode
      lblFile <- ContT $ withFile lbln WriteMode
      liftIO $ do
        hPutStr mifFile "DEPTH = 16384;\r\nWIDTH = 32;\r\nADDRESS_RADIX = DEC;\r\nDATA_RADIX = DEC;\r\nCONTENT\r\nBEGIN\r\n"
        mapM_ (hPutStr mifFile) $ map (uncurry wordToMIF) $ zip [0..] bytecode
        hPutStr mifFile "END;\r\n \r\n\r\n"
        mapM_ (hPutStrLn lblFile . uncurry wordToLBL) $ sortBy lblOrdering $ HM.toList $ st^.pstLbls

      case crossname args of
        Just crossn -> do
          crossFile <- ContT $ withFile crossn WriteMode
          liftIO $ mapM_ (hPutStr crossFile . wordToE) $ bytecode
        Nothing -> return ()

  where
    wordToMIF :: Int -> Word32 -> String
    wordToMIF i v = "\t" ++ show i ++ "\t:\t" ++ show (unsafeCoerce v :: Int32) ++ ";\r\n"
    wordToLBL :: String -> Word32 -> String
    wordToLBL lbl val = lbl ++ "\t" ++ show val ++ "\t" ++ "(0x" ++ showHex val ")"
    wordToE :: Word32 -> String
    wordToE v = "  " ++ show v ++ "\n"
    lblOrdering :: (String, Word32) -> (String, Word32) -> Ordering
    lblOrdering (_, a) (_, b) = compare a b
