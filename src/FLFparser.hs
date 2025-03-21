module FLFparser where

import Text.Parsec (ParseError, parse, try, anyChar)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, string, letter, satisfy)
import Text.Parsec.Combinator (eof, many1, choice, chainl1, between, count, option, optionMaybe, optional, sepBy, sepEndBy, lookAhead)
import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
import Control.Monad (void, ap, mzero)
import Data.Char (isLetter, isDigit, isUpper, isLower)
import Data.Maybe (fromMaybe)
import Data.Foldable (sequenceA_, traverse_)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \r\n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

spaceSep :: Parser a -> Parser a
spaceSep p = do
    x <- p
    void $ many $ char ' '
    return x

digits :: Parser Int
digits = lexeme $ do
    ds <- many1 $ satisfy isDigit
    return $ read ds

parseHeader :: Parser FLFConfig
parseHeader = do
    void $ spaceSep $ string "flf2a" 

    hardblank <- spaceSep anyChar

    return $ FLFConfig hardblank 10

data FLFFile 
    = FLFFile FLFConfig FLFChar
    deriving (Show)

data FLFConfig
    = FLFConfig Char Int
    deriving (Show)

data FLFChar
    = FLFChar Int [String]
    deriving (Show)

















