{-# LANGUAGE TemplateHaskell #-}
module Parser where

import Unsafe.Coerce -- teehee
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec.Error
import Control.Arrow
import Text.Megaparsec.State
import Control.Lens hiding (noneOf)
import Data.Void
import Data.List
import Data.Maybe
import Text.Megaparsec hiding (Label)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.StaticHash as SH
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as E
import Data.Word
import Data.Int

type LabelMap = HM.HashMap String Word32
type SrcMap = HM.HashMap String SrcInfo
type InternalPEB = ParseErrorBundle T.Text Void
type Parser = ParsecT Void T.Text (StateT PState IO)

data SrcInfo = SrcInfo {
  srcInclusionOffset :: Int,
  srcPosState :: PosState T.Text }
  deriving Show

data PState = PState {
  _pstAddr :: Word32,
  _pstLbls :: LabelMap,
  _pstErrs :: [InternalPEB],
  _pstSrcs :: SrcMap }
  deriving Show
makeLenses ''PState

newPState :: PState
newPState = PState 0 HM.empty [] HM.empty

data Value = VInt Word32 | VLabel String | VConstExpr Value Value (Word32 -> Word32 -> Word32)

instance Show Value where
  show (VInt n) = "VInt " ++ show n
  show (VLabel lbl) = "VLabel " ++ show lbl
  show (VConstExpr v1 v2 _) = "VConstExpr (" ++ show v1 ++ " ? " ++ show v2 ++ ")"

data Instr = Instr Word32 Value Value Value
  deriving Show
data Chunk = CInstr Instr | Lit Value | Padding Word32
  deriving Show

data ChunkOffset = COLocal Int | CORemote Int String
  deriving Show

applyRemote :: String -> ChunkOffset -> ChunkOffset
applyRemote _ c@(CORemote _ _) = c -- remote files don't get more remote
applyRemote src (COLocal offset) = CORemote offset src

instrOpcodes :: SH.StaticHash String Word32
instrOpcodes = SH.fromList [
  ("halt", 0),
  ("add", 1),
  ("sub", 2),
  ("mult", 3),
  ("div", 4),
  ("cp", 5),
  ("and", 6),
  ("or", 7),
  ("not", 8),
  ("sl", 9),
  ("sr", 10),
  ("cpfa", 11),
  ("cpta", 12),
  ("be", 13),
  ("bne", 14),
  ("blt", 15),
  ("call", 16),
  ("ret", 17)]

vzero :: Value
vzero = VInt 0

size :: Chunk -> Word32
size (CInstr _) = 4
size (Lit _) = 1
size (Padding paddingSize) = paddingSize

parse :: String -> IO ((Either InternalPEB [Word32], PState))
parse fname = do
  ftext <- TIO.readFile fname
  runStateT (runParserT parser fname ftext) newPState

parser :: Parser [Word32]
parser = do
  chunks <- chunker
  lbls <- _pstLbls <$> get
  concat <$> mapM (uncurry $ evalChunk lbls) chunks
  where
    evalChunk :: LabelMap -> ChunkOffset -> Chunk -> Parser [Word32]
    evalChunk _ _ (Padding pad) = return $ replicate (fromIntegral pad) 0
    evalChunk lbls offset (Lit val) = singleton <$> evalValue lbls offset val
    evalChunk lbls offset (CInstr (Instr opcode v1 v2 v3)) = sequence $
      return opcode : evalValue lbls offset v1 : evalValue lbls offset v2 : evalValue lbls offset v3 : []
    evalValue :: LabelMap -> ChunkOffset -> Value -> Parser Word32
    evalValue _ _ (VInt n) = return n
    evalValue lbls offset (VLabel lbl) = case HM.lookup lbl lbls of
      Just addr -> return addr
      Nothing -> errAtOffset lbl offset
    evalValue lbls offset (VConstExpr v1 v2 opr) =
      opr <$> evalValue lbls offset v1 <*> evalValue lbls offset v2
    errAtOffset :: String -> ChunkOffset -> Parser a
    errAtOffset lbl (COLocal offset) = region (setErrorOffset offset) $
      fail $ "label \"" ++ lbl ++ "\" doesn't exist"
    errAtOffset lbl (CORemote offset src) = do
      srcs <- _pstSrcs <$> get
      let srcInfo = srcs HM.! src
      let remoteError = FancyError offset (E.singleton . ErrorFail $ "label \"" ++ lbl ++ "\" doesn't exist")
      pstErrs %= (:) (ParseErrorBundle (NE.singleton remoteError) $ srcPosState srcInfo)
      region (setErrorOffset $ srcInclusionOffset srcInfo) $
        fail $ "error in included file \"" ++ src ++ "\""

eol :: Parser ()
eol = eof <|> (C.eol >> return ())

chunker :: Parser [(ChunkOffset, Chunk)]
chunker = concat <$> manyTill
  (try parseDirective <|> try parseLine <|> (whitespace >> eol >> return [])) eof
  where
    parseLine :: Parser [(ChunkOffset, Chunk)]
    parseLine = do
      (optional $ try $ L.nonIndented whitespace safeIdentifier) >>= \x ->
        case x of
          Just lbl -> do
            st <- get
            if HM.member lbl (st^.pstLbls) then fail $ "duplicate label " ++ lbl
            else pstLbls %= HM.insert lbl (st^.pstAddr)
          Nothing -> return ()

      whitespace

      offset <- getOffset
      chunks <- try (singleton . ((,) $ COLocal offset) . CInstr <$> parseInstr) <|> try parseNonInstr
      -- increment addr based on chunk sizes
      mapM (\c -> ((pstAddr %= (+) (size $ snd c)) *> return c)) chunks <* eol
    parseNonInstr :: Parser [(ChunkOffset, Chunk)]
    parseNonInstr = do
      fmap concat $ many $ do
        offset <- getOffset
        fmap ((,) (COLocal offset) <$>) $
          try (singleton . Lit <$> parseValue)
          <|> try parseString
          <|> try (singleton . Padding <$> parsePadding)

parseDirective :: Parser [(ChunkOffset, Chunk)]
parseDirective = do
  _ <- lexeme $ C.char '#' >> L.symbol whitespace (T.pack "include")
  offset <- getOffset
  filename >>= \fname -> do
    ftext <- liftIO $ TIO.readFile fname
    let posState = initialPosState fname ftext
    pstSrcs %= HM.insert fname (SrcInfo offset posState)
    st <- get
    (parseResult, newst) <- liftIO $ runStateT (runParserT chunker fname ftext) st
    put newst
    case parseResult of
      Left peb ->
        (pstErrs %= (:) peb)
          >> region (setErrorOffset offset) (fail $ "error in included file \"" ++ fname ++ "\"")
      Right chunks -> return $ map (first $ applyRemote fname) $ chunks
  where
    filename :: Parser String
    filename = manyTill anySingle eol

parsePadding :: Parser Word32
parsePadding = lexeme . label "padding" $ C.char '$' >> do
  offset <- getOffset
  number >>= checkIntBounds offset

-- Convenience types in the form of "some characters" which become just a string of values
parseString :: Parser [Chunk]
parseString =
  lexeme . label "string literal" $ (Lit. VInt . fromIntegral . fromEnum <$>)
    <$> between (C.char '"') (C.char '"') (many $ noneOf "\"")

parseInstr :: Parser Instr
parseInstr = lexeme (label "instruction" identifier) >>= \raw_instr -> do
  case raw_instr of
    "halt" -> return $ Instr 0 vzero vzero vzero
    "add" -> ternary 1
    "sub" -> ternary 2
    "mult" -> ternary 3
    "div" -> ternary 4
    "cp" -> binary 5
    "and" -> ternary 6
    "or" -> ternary 7
    "not" -> binary 8
    "sl" -> ternary 9
    "sr" -> ternary 10
    "cpfa" -> ternary 11
    "cpta" -> ternary 12
    "be" -> ternary 13
    "bne" -> ternary 14
    "blt" -> ternary 15
    "call" -> binary 16
    "ret" -> unary 17
    opr -> fail $ "unknown operation" ++ opr
  where
    ternary opcode = Instr opcode <$> parseValue <*> parseValue <*> parseValue
    binary opcode = Instr opcode <$> parseValue <*> parseValue <*> return vzero
    unary opcode = Instr opcode <$> parseValue <*> return vzero <*> return vzero

identifier :: Parser String
identifier = try $ (:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_')

safeIdentifier :: Parser String
safeIdentifier = label "valid identifier" $ identifier >>= \ident ->
  if isNothing $ SH.lookup ident instrOpcodes then return ident
  else fail $ "reserved identifier" ++ ident

whitespace :: Parser ()
whitespace = L.space
  C.hspace1
  (L.skipLineComment $ T.pack "//")
  (L.skipBlockComment (T.pack "/*") (T.pack "*/"))

-- wraps with local whitespace
lexeme :: Parser v -> Parser v
lexeme = L.lexeme whitespace

parseValue :: Parser Value
parseValue = lexeme . label "const value" $
  try charLit
    <|> (do
      offset <- getOffset
      fmap VInt $ checkIntBounds offset =<< try number)
    <|> try expr
    <|> (VLabel <$> try safeIdentifier)
  where
    charLit :: Parser Value
    charLit =
      label "character literal" $ VInt . fromIntegral . fromEnum
        <$> between (C.char '\'') (C.char '\'') L.charLiteral
    expr :: Parser Value
    expr = label "constant expression" . between (C.char '(') (C.char ')') $ do
      v1 <- parseValue
      opr <- lexeme operator
      v2 <- parseValue
      return $ VConstExpr  v1 v2 opr
    operator :: Parser (Word32 -> Word32 -> Word32)
    operator = (C.char '+' >> return (+)) <|> (C.char '-' >> return (-))

number :: Parser Integer
number = try hexLit <|> try decLit
  where
    decLit :: Parser Integer
    decLit = label "decimal literal" $ (fmap negate $ C.char '-' >> L.decimal) <|> L.decimal
    hexLit :: Parser Integer
    hexLit = label "hexadecimal literal" $ (C.char '0' >> C.char 'x' >> L.hexadecimal)

checkIntBounds :: Int -> Integer -> Parser Word32
checkIntBounds offset num -- takes offset as a param so that errors come at the front of the number
  | num <= fromIntegral (maxBound :: Word32) && num >= fromIntegral (minBound :: Word32) =
    return $ fromIntegral num
    -- hehehe i love "unsafe" code :)))))
  | num >= fromIntegral (minBound :: Int32) = return $ unsafeCoerce $ (fromIntegral num :: Int32)
  | otherwise = do
    -- required to bypass alternatives; a number can't be interpreted any other way ˉ\(ツ)/ˉ
    -- basically a janky hack :)
    registerParseError $ FancyError offset $ E.singleton $ ErrorFail "number too large, must fit within 32 bits"
    -- this is fine, since the parser is guaranteed to fail (an error exists)
    return 0
