{-# LANGUAGE TemplateHaskell #-}
module Parser where

import System.Exit
import System.IO.Error
import qualified Control.Exception as EX
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

-- Values are either literal numbers, references to labels, or expressions made up of
-- two values and a combining operation.
data Value = VInt Word32 | VLabel String | VConstExpr Value Value (Word32 -> Word32 -> Word32)
-- for debugging
instance Show Value where
  show (VInt n) = "VInt " ++ show n
  show (VLabel lbl) = "VLabel " ++ show lbl
  show (VConstExpr v1 v2 _) = "VConstExpr (" ++ show v1 ++ " ? " ++ show v2 ++ ")"

-- This actually holds both labels and define directives in one place:
-- Left holds labels (with literal addresses)
-- Right holds values (from define directives)
-- Labels can't be converted to values because trying to parse a value out of context
-- (like when generating a .labels file)
-- could possibly generate parse errors, so they have to be stored as literals.
-- Small side effect: define directives that aren't used anywhere can actually have
-- invalid labels in their definitions! Since the value never gets parsed, it'll never be discovered.
-- Oops! Should probably fix that at some point, but it's not a breaking bug.
type LabelMap = HM.HashMap String (Either Word32 Value)

-- This holds all the information needed to tie parse errors happening in included files
-- to the proper file. Due to how megaparsec handles errors, ParseErrorBundles are made up of
-- several errors with integer offsets and a single source info which gives the source in its
-- entirety. This means that any individual error doesn't have any record of where its from,
-- so we have to keep track of that manually.
type SrcMap = HM.HashMap String SrcInfo
-- ParseErrorBundle parameterised by our constraints
type InternalPEB = ParseErrorBundle T.Text Void

-- Instr opcode arg1 arg2 arg3
data Instr = Instr Word32 Value Value Value
  deriving Show

-- Chunks are units which represent the smallest units of the language.
-- Padding is included here for efficiency.
data Chunk = CInstr Instr | Lit Value | Padding Word32
  deriving Show

-- Keeps track of where a chunk came from in the source for proper error handling
-- during the second pass. Local offsets come from the actively parsed file. At the top
-- level, this is the source file being parsed. Remote offsets are generated when including
-- a file; when parsing these all local offsets are converted to remote offsets.
data ChunkOffset = COLocal Int | CORemote Int String
  deriving Show

-- Inclusion offset is here so that we can generate a reasonable error in the file in which
-- they were included. The PosState is what the ParseErrorBundle needs for context.
data SrcInfo = SrcInfo {
  _srcInclusionOffset :: ChunkOffset,
  _srcPosState :: PosState T.Text }
  deriving Show
makeLenses ''SrcInfo

data PState = PState {
  _pstAddr :: Word32, -- current address, monotonically increases across parsing
  _pstLbls :: LabelMap, -- holds labels and define directives
  _pstErrs :: [InternalPEB], -- holds remote file ParseErrorBundles
  _pstSrcs :: SrcMap } -- contains information on sources for linking with PEBs
  deriving Show
makeLenses ''PState

-- Why this monad stack?
-- 1. IO is necessary for including files.
-- 2. StateT is necessary for holding the parser's context; it has to go outside of
--    ParsecT because it holds all ParseErrorBundles for remote files. Due to how ParsecT
--    works, it can only be considered to parsing one source file at a time, so other file's
--    errors have to be stored elsewhere (in PState). An error in an included file generates
--    an error in the main file which would invalidate PState if it were within ParsecT.
-- 3. ParsecT is necessary for obvious reasons.
type Parser = ParsecT Void T.Text (StateT PState IO)

-- Convenience
newPState :: PState
newPState = PState 0 HM.empty [] HM.empty

-- Converts the local offsets to remote offsets by applying a specific source.
applyRemote :: String -> ChunkOffset -> ChunkOffset
applyRemote _ c@(CORemote _ _) = c -- remote files don't get more remote
applyRemote src (COLocal offset) = CORemote offset src

-- The values here are just dummy data; this is basically a hash set of reserved keywords.
instrOpcodes :: SH.StaticHash String Word32
instrOpcodes = SH.fromList [
  ("halt", 0),
  ("add", 1),
  ("sub", 2),
  ("mult", 3),
  ("div", 4),
  ("cp", 5),
  ("cpdata", 5),
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

-- Useful shorthand for parseInstr
vzero :: Value
vzero = VInt 0

-- Calculate a chunk's size; every chunk must have a trivial size computable on the first pass
-- so that labels can be solved immediately.
size :: Chunk -> Word32
size (CInstr _) = 4
size (Lit _) = 1
size (Padding paddingSize) = paddingSize

-- Convenience shorthand for erring at offsets
regionErrOffset :: Int -> String -> Parser a
regionErrOffset offset = region (setErrorOffset offset) . fail

-- Top level parsing, called from main
-- `die`s when it can't read the file
parse :: String -> IO ((Either InternalPEB [Word32], PState))
parse fname = (EX.try $ TIO.readFile fname) >>= (either (die . fileErrorText fname) $
  \ftext -> runStateT (runParserT parser fname ftext) newPState)

fileErrorText :: String -> IOError -> String
fileErrorText fname e
  | isDoesNotExistError e = "file \"" ++ fname ++ "\" does not exist"
  | isPermissionError e = "not permitted to open file \"" ++ fname ++ "\""
  | isAlreadyInUseError e = "file \"" ++ fname ++ "\" is already in use"
  | otherwise = "could not open file \"" ++ fname ++ "\""

-- Root parser, compiles chunks into words
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
    -- evaluates values by pulling values from the LabelMap
    evalValue :: LabelMap -> ChunkOffset -> Value -> Parser Word32
    evalValue _ _ (VInt n) = return n
    evalValue lbls offset (VLabel lbl) = case HM.lookup lbl lbls of
      Just (Right val) -> evalValue lbls offset val -- eval define directive values
      Just (Left addr) -> return addr -- simple label address
      Nothing -> errAtOffset ("label \"" ++ lbl ++ "\" doesn't exist") offset -- missing label
    evalValue lbls offset (VConstExpr v1 v2 opr) =
      opr <$> evalValue lbls offset v1 <*> evalValue lbls offset v2
    -- generates an error at the appropriate offset for a specific chunk
    -- this includes handling generating all necessary errors in the case of a remote file
    errAtOffset :: String -> ChunkOffset -> Parser a
    errAtOffset err (COLocal offset) = regionErrOffset offset err
    errAtOffset err (CORemote offset src) = do
      srcs <- _pstSrcs <$> get
      let srcInfo = srcs HM.! src
      let remoteError = FancyError offset (E.singleton . ErrorFail $ err)
      pstErrs %= (:) (ParseErrorBundle (NE.singleton remoteError) $ _srcPosState srcInfo)
      errAtOffset ("error in included file \"" ++ src ++ "\"") $ _srcInclusionOffset srcInfo

-- end of line or end of input; convenience
eol :: Parser ()
eol = eof <|> (C.eol >> return ())

-- generates chunks from sources; the real meat of the parsing
chunker :: Parser [(ChunkOffset, Chunk)]
chunker = concat <$> manyTill
  (try parseDirective <|> try parseLine <|> (whitespace >> eol >> return [])) eof
  where
    parseLine :: Parser [(ChunkOffset, Chunk)]
    parseLine = do
      -- attempts to parse a label
      lblOffset <- getOffset -- save the offset for cleaner error reporting
      (optional $ try $ L.nonIndented whitespace safeIdentifier) >>= \x ->
        case x of
          Just lbl -> do
            st <- get
            if HM.member lbl (st^.pstLbls) then
              -- errs in the event a label has already been defined
              regionErrOffset lblOffset $ "duplicate label " ++ lbl
            else pstLbls %= HM.insert lbl (Left $ st^.pstAddr)
          Nothing -> return ()

      whitespace

      -- parses and applies offset appropriately to instruction chunks
      offset <- getOffset
      chunks <- try (singleton . ((,) $ COLocal offset) . CInstr <$> parseInstr) <|> try parseNonInstr
      -- increment addr based on chunk sizes
      mapM (\c -> ((pstAddr %= (+) (size $ snd c)) *> return c)) chunks <* eol
    -- collect many values into multiple chunks
    parseNonInstr :: Parser [(ChunkOffset, Chunk)]
    parseNonInstr = do
      fmap concat $ many $ do
        offset <- getOffset
        fmap ((,) (COLocal offset) <$>) $
          try (singleton . Lit <$> parseValue)
          <|> try parseString
          <|> try (singleton . Padding <$> parsePadding)

-- anything following a `#`
parseDirective :: Parser [(ChunkOffset, Chunk)]
parseDirective = label "directive" $
  C.char '#' >> (try parseIncludeDirective <|> try parseDefineDirective)

-- #include
parseIncludeDirective :: Parser [(ChunkOffset, Chunk)]
parseIncludeDirective = label "include directive" $ do
  _ <- L.symbol whitespace (T.pack "include")
  offset <- getOffset
  fname <- lexeme filename
  possible <- liftIO . EX.try $ TIO.readFile fname
  case possible of
    Left e -> regionErrOffset offset $ fileErrorText fname e
    Right ftext -> do
      let posState = initialPosState fname ftext
      pstSrcs %= HM.insert fname (SrcInfo (COLocal offset) posState)
      st <- get
      (parseResult, newst) <- liftIO $ runStateT (runParserT chunker fname ftext) (pstSrcs .~ HM.empty $ st)
      put newst
      -- make all new source offsets remote
      pstSrcs %= HM.map (srcInclusionOffset %~ applyRemote fname)
      -- combine with the old sources, keeping old sources when given the option
      -- this has the effect of source-inclusion errors always being reported at the first instance of inclusion
      pstSrcs %= HM.union (_pstSrcs st)
      case parseResult of
        -- if there's a parse error, generate one in this file at the point of inclusion
        Left peb -> 
          (pstErrs %= (:) peb)
            >> regionErrOffset offset (fail $ "error in included file \"" ++ fname ++ "\"")
        -- turn all local chunk offsets in this file into remote offsets
        Right chunks -> return $ map (first $ applyRemote fname) $ chunks
  where
    -- validation? what's that
    filename :: Parser String
    filename = manyTill anySingle eol

-- #define
parseDefineDirective :: Parser [(ChunkOffset, Chunk)]
parseDefineDirective = label "define directive" $ do
  _ <- L.symbol whitespace (T.pack "define")
  -- save offset for better error reporting
  lblOffset <- getOffset
  lbl <- lexeme safeIdentifier
  val <- parseValue
  st <- get
  if HM.member lbl $ st^.pstLbls then
    regionErrOffset lblOffset $ "duplicate label \"" ++ lbl ++ "\""
  else
  -- create new "label" - see LabelMap
    pstLbls %= HM.insert lbl (Right val)

  -- doesn't actually do anything to the source
  return []

parsePadding :: Parser Word32
parsePadding = lexeme . label "padding" $ C.char '$' >> do
  offset <- getOffset
  number >>= checkIntBounds offset

-- Convenience types in the form of "some characters" which become just a string of values
parseString :: Parser [Chunk]
parseString =
  lexeme . label "string literal" $ (Lit. VInt . fromIntegral . fromEnum <$>)
    <$> ((C.char '"') >> (manyTill L.charLiteral $ C.char '"'))

parseInstr :: Parser Instr
parseInstr = lexeme (label "instruction" identifier) >>= \raw_instr -> do
  case raw_instr of
    "halt" -> return $ Instr 0 vzero vzero vzero
    "add" -> ternary 1
    "sub" -> ternary 2
    "mult" -> ternary 3
    "div" -> ternary 4
    "cp" -> binary 5
    "cpdata" -> do -- complicated because we have to do some wacky stuff with addresses
      target <- parseValue
      val <- parseValue
      st <- get
      -- we can do this because when parsing instructions, addr will always be up to date
      return $ Instr 5 target (VInt $ st^.pstAddr + 3) val
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

-- identifiers in the form of a letter followed by letters, numbers, or underscores
-- note: maybe consider allowing initial underscores
identifier :: Parser String
identifier = try $ (:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '_')

-- identifier which isn't a reserved keyword (the instructions)
safeIdentifier :: Parser String
safeIdentifier = label "valid identifier" $ identifier >>= \ident ->
  if isNothing $ SH.lookup ident instrOpcodes then return ident
  else fail $ "reserved identifier" ++ ident

-- spaces and comments
whitespace :: Parser ()
whitespace = L.space
  C.hspace1
  (L.skipLineComment $ T.pack "//")
  (L.skipBlockComment (T.pack "/*") (T.pack "*/"))

-- wraps with local whitespace
lexeme :: Parser v -> Parser v
lexeme = L.lexeme whitespace

-- reads a value: either a label, an expression, or a literal
parseValue :: Parser Value
parseValue = lexeme . label "const value" $
    (do
      offset <- getOffset -- offset for properly reporting number errors
      fmap VInt $ checkIntBounds offset =<< try number)
    <|> try expr
    <|> (VLabel <$> try safeIdentifier)
  where
    -- parse an expression surrounded by parentheses
    expr :: Parser Value
    expr = label "constant expression" . between (C.char '(') (C.char ')') $ do
      v1 <- parseValue
      opr <- lexeme operator
      v2 <- parseValue
      return $ VConstExpr  v1 v2 opr
    -- parse the two appropriate operators
    operator :: Parser (Word32 -> Word32 -> Word32)
    operator = (C.char '+' >> return (+)) <|> (C.char '-' >> return (-))

-- literal number; somewhat of a misnomer since char literals are also allowed
-- they just get converted to numbers
number :: Parser Integer
number = try hexLit <|> try decLit <|> try charLit
  where
    decLit :: Parser Integer
    decLit = label "decimal literal" $ (fmap negate $ C.char '-' >> L.decimal) <|> L.decimal
    hexLit :: Parser Integer
    hexLit = label "hexadecimal literal" $ (C.char '0' >> C.char 'x' >> L.hexadecimal)
    charLit :: Parser Integer
    charLit =
      label "character literal" $ fromIntegral . fromEnum
        <$> between (C.char '\'') (C.char '\'') L.charLiteral

-- ensure that a number is within the appropriate bounds, and throw an error
-- (at the appropriate offset) if it's not
-- numbers will be implicitly treated as unsigned/signed, as in numbers too large to be
-- considered signed but small enough to be unsigned will be treated as unsigned and
-- negative numbers will be treated as signed
-- this is fine since the positive signed numbers are representationally equivalent to their
-- unsigned counterparts, and operations like addition and subtraction are all valid for both
-- types of numbers
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

