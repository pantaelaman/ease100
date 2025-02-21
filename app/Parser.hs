module Parser where

import Data.Void
import Text.Megaparsec.Debug
import Data.List
import Data.Maybe
import Text.Megaparsec hiding (Label)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.StaticHash as SH
import Control.Arrow
import qualified Data.Text as T

type LabelMap = HM.HashMap String Int
type InternalParser = Parsec Void T.Text
type InternalPEB = ParseErrorBundle T.Text Void
type Parser = StateT (Int, LabelMap) InternalParser

data Value = VInt Int | VLabel String | VConstExpr Value Value (Int -> Int -> Int)

instance Show Value where
  show (VInt n) = "VInt " ++ show n
  show (VLabel lbl) = "VLabel " ++ show lbl
  show (VConstExpr v1 v2 _) = "VConstExpr (" ++ show v1 ++ " ? " ++ show v2 ++ ")"

data Instr = Instr Int Value Value Value
  deriving Show
data Chunk = CInstr Instr | Lit Value | Padding Int
  deriving Show

instrOpcodes :: SH.StaticHash String Int
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

size :: Chunk -> Int
size (CInstr _) = 4
size (Lit _) = 1
size (Padding paddingSize) = paddingSize

parse :: T.Text -> Either InternalPEB ([Int], LabelMap)
parse = runParser (second snd <$> runStateT parser (0, HM.empty)) "fname"

parser :: Parser [Int]
parser = do
  chunks <- chunker
  (_, lbls) <- get
  concat <$> mapM (uncurry $ evalChunk lbls) chunks
  where
    evalChunk :: LabelMap -> Int -> Chunk -> Parser [Int]
    evalChunk _ _ (Padding pad) = return $ replicate pad 0
    evalChunk lbls offset (Lit val) = singleton <$> evalValue lbls offset val
    evalChunk lbls offset (CInstr (Instr opcode v1 v2 v3)) = sequence $
      return opcode : evalValue lbls offset v1 : evalValue lbls offset v2 : evalValue lbls offset v3 : []
    evalValue :: LabelMap -> Int -> Value -> Parser Int
    evalValue _ _ (VInt n) = return n
    evalValue lbls offset (VLabel lbl) = case HM.lookup lbl lbls of
      Just addr -> return addr
      Nothing -> region (setErrorOffset offset) $ fail $ "label \"" ++ lbl ++ "\" doesn't exist"
    evalValue lbls offset (VConstExpr v1 v2 op) = op <$> evalValue lbls offset v1 <*> evalValue lbls offset v2

eol :: Parser ()
eol = eof <|> (C.eol >> return ())

chunker :: Parser [(Int, Chunk)]
chunker = dbg "here" $ concat <$> manyTill (try parseLine <|> (lift whitespace >> eol >> return [])) eof
  where
    parseLine :: Parser [(Int, Chunk)]
    parseLine = dbg "str" $ do
      (optional $ try $ lift $ L.nonIndented whitespace safeIdentifier) >>= \x ->
        case x of
          Just lbl -> do
            (addr, lbls) <- get
            if HM.member lbl lbls then fail $ "duplicate label " ++ lbl
            else put (addr, HM.insert lbl addr lbls)
          Nothing -> return ()

      lift whitespace

      offset <- getOffset
      chunks <- try (singleton . ((,) offset) . CInstr <$> lift parseInstr) <|> try parseNonInstr
      -- increment addr based on chunk sizes
      mapM (\c -> (modify . first . (+) . size $ snd c) *> return c) chunks <* eol
    parseNonInstr :: Parser [(Int, Chunk)]
    parseNonInstr = do
      fmap concat $ many $ do
        offset <- getOffset
        fmap ((,) offset <$>) $
          try (singleton . Lit <$> lift parseValue)
          <|> try (lift parseString)
          <|> try (singleton . Padding <$> lift parsePadding)

parsePadding :: InternalParser Int
parsePadding = lexeme . label "padding" $ C.char '$' >> number

-- Convenience types in the form of "some characters" which become just a string of values
parseString :: InternalParser [Chunk]
parseString =
  lexeme . label "string literal" $ (Lit. VInt . fromEnum <$>) <$> between (C.char '"') (C.char '"') (many $ noneOf "\"")

parseInstr :: InternalParser Instr
parseInstr = lexeme (label "operation" identifier) >>= \raw_instr -> do
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
    op -> fail $ "unknown operation" ++ op
  where
    ternary opcode = Instr opcode <$> parseValue <*> parseValue <*> parseValue
    binary opcode = Instr opcode <$> parseValue <*> parseValue <*> return vzero
    unary opcode = Instr opcode <$> parseValue <*> return vzero <*> return vzero

identifier :: InternalParser String
identifier = try $ (:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_')

safeIdentifier :: InternalParser String
safeIdentifier = label "valid identifier" $ identifier >>= \ident ->
  if isNothing $ SH.lookup ident instrOpcodes then return ident
  else fail $ "reserved identifier" ++ ident

whitespace :: InternalParser ()
whitespace = L.space
  C.hspace1
  (L.skipLineComment $ T.pack "//")
  (L.skipBlockComment (T.pack "/*") (T.pack "*/"))

-- wraps with local whitespace
lexeme :: InternalParser v -> InternalParser v
lexeme = L.lexeme whitespace

parseValue :: InternalParser Value
parseValue = lexeme . label "const value" $ try charLit <|> try (VInt <$> number) <|> try expr <|> try (VLabel <$> safeIdentifier)
  where
    charLit :: InternalParser Value
    charLit = label "character literal" $ VInt . fromEnum <$> between (C.char '\'') (C.char '\'') L.charLiteral
    expr :: InternalParser Value
    expr = label "constant expression" . between (C.char '(') (C.char ')') $ do
      v1 <- parseValue
      op <- lexeme operator
      v2 <- parseValue
      return $ VConstExpr  v1 v2 op
    operator :: (MonadParsec e s m, Token s ~ Char) => m (Int -> Int -> Int)
    operator = (C.char '+' >> return (+)) <|> (C.char '-' >> return (-))

number :: InternalParser Int
number = try hexLit <|> try decLit
  where
    decLit :: InternalParser Int
    decLit = label "decimal literal" $ L.decimal
    hexLit :: InternalParser Int
    hexLit = label "hexadecimal literal" $ (C.char '0' >> C.char 'x' >> L.hexadecimal)
