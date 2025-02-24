module Main where

import System.Exit
import Control.Exception
import Data.Tuple.Extra (secondM)
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

-- cli args
data Args = Args
  { fname :: String -- input filename
  , mifname :: Maybe String -- .mif filename
  , lblname :: Maybe String -- .labels filename
  , crossname :: Maybe String } -- .e filename, only generated if Just

argParser :: O.Parser Args
argParser = Args
  <$> O.strArgument (O.metavar "FILE")
  <*> optional (O.strOption
    (  O.metavar "FILE"
    <> O.short 'm'
    <> O.long "mif"
    <> O.help "Specify .mif filepath"))
  <*> optional (O.strOption
    (  O.metavar "FILE"
    <> O.short 'l'
    <> O.long "labels"
    <> O.help "Specify .labels filepath"))
  <*> optional (O.strOption
    (  O.metavar "FILE"
    <> O.short 'e'
    <> O.long "cross"
    <> O.help "Generate a .e file"))

-- TODO: add more description
argInfo :: O.ParserInfo Args
argInfo = O.info (argParser <**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Compiles an extended ase100 file, generating .mif and .labels files.\nOptionally generates a .e file."
  <> O.header "ease - a compiler for extended ase100 programs")

main :: IO ()
main = do
  args <- O.execParser argInfo
  -- generate default .mif and .labels filenames from input
  let sname = reverse . dropWhile (/= '.') . reverse $ fname args -- drops from the end until reaching the last period
  let mifn = fromMaybe (sname ++ "mif") $ mifname args -- .mif filename, either default or provided
  let lbln = fromMaybe (sname ++ "labels") $ lblname args -- .labels filename, either default or provided
  (parseResult, st) <- parse $ fname args -- grab parser and final state (for remote errors and labels)

  -- pretty print remote errors, does nothing if no remote errors are generated
  mconcat $ (putStrLn . MP.errorBundlePretty) <$> st^.pstErrs
  case parseResult of
    -- pretty print local errors
    -- a local error is guaranteed to be generated if a remote error is generated
    Left err -> putStr $ MP.errorBundlePretty err
    -- write bytecode to appropriate file formats
    Right bytecode -> do
      handle (die . fileErrorText mifn) $ withFile mifn WriteMode $ \mifFile -> do
        -- mif header
        hPutStr mifFile "DEPTH = 16384;\r\nWIDTH = 32;\r\nADDRESS_RADIX = DEC;\r\nDATA_RADIX = DEC;\r\nCONTENT\r\nBEGIN\r\n"
        mapM_ (hPutStr mifFile) $ map (uncurry wordToMIF) $ zip [0..] bytecode
        -- mif tail
        hPutStr mifFile "END;\r\n \r\n\r\n"

      handle (die . fileErrorText lbln) $ withFile lbln WriteMode $ \lblFile ->
        -- dump to label file
        mapM_ (hPutStrLn lblFile . uncurry wordToLBL) $ sortBy lblOrdering $
          -- only dump labels (Left) not the define directives (Right)
          catMaybes $ map (secondM $ either (Just) (const Nothing)) $ HM.toList $ st^.pstLbls

      case crossname args of
        -- create .e file if requested
        Just crossn -> do
          handle (die . fileErrorText crossn) $ withFile crossn WriteMode $ \crossFile ->
            mapM_ (hPutStr crossFile . wordToE) $ bytecode
        Nothing -> return ()

  where
    -- various representations of words in the file formats
    wordToMIF :: Int -> Word32 -> String
    wordToMIF i v = "\t" ++ show i ++ "\t:\t" ++ show (unsafeCoerce v :: Int32) ++ ";\r\n"
    wordToLBL :: String -> Word32 -> String
    wordToLBL lbl val = lbl ++ "\t" ++ show val ++ "\t" ++ "(0x" ++ showHex val ")"
    wordToE :: Word32 -> String
    wordToE v = "  " ++ show (unsafeCoerce v :: Int32) ++ "\n"
    lblOrdering :: (String, Word32) -> (String, Word32) -> Ordering
    lblOrdering (_, a) (_, b) = compare a b
