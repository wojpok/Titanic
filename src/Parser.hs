{-# LANGUAGE MultilineStrings #-}

module Parser ( StyleValue (..)
              , StyleDict (..)
              , DocNode (..)
              , DocNodeSR (..)
              , NodeStructure (..)
              , Interp (..)
              , SR (..)
              , parseStyle
              ) where

import Text.Parsec (ParseError, parse, try, anyChar, alphaNum)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, string, letter, satisfy)
import Text.Parsec.Combinator (eof, many1, choice, chainl1, between, count, option, optionMaybe, optional, sepBy, sepEndBy, lookAhead)
import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>), many)
import Control.Monad (void, ap, mzero)
import Data.Char (isLetter, isDigit, isUpper, isLower)
import Data.Maybe (fromMaybe)
import Data.Foldable (sequenceA_, traverse_)

import Types

data StyleValue
  = SString String
  | SBool Bool
  | SInt Int
  | SColor Color
  deriving (Show, Eq, Ord)

newtype StyleDict = StyleDict { getStyleDict :: [(String, StyleValue)] }
  deriving Show

data DocNode 
  = DocNode String (Maybe StyleValue) StyleDict NodeStructure
  deriving Show

newtype DocNodeSR = DocNodeSR { getDocNodeSR :: SR DocNode }
  deriving Show

data NodeStructure 
  = NSRecs [DocNodeSR]
  | NSInter [Interp]
  deriving Show

data Interp
  = IString (SR String)
  | INode Int DocNodeSR
  deriving Show

data SR a 
  = S (SR a)
  | R (SR a)
  | E a
  deriving Show

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (whitespace *> p <* eof) ""

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \r\n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

lexoid :: Parser a -> Parser ()
lexoid p = p *> whitespace

digits :: Parser Int
digits = do
    ds <- many1 $ satisfy isDigit
    return $ read ds

parseStyleValue :: Parser StyleValue
parseStyleValue = lexeme $ (parseString <|> parseBool <|> parseInt <|> parseColor <|> parseIdent)
  where
    parseString = char '\'' *> (SString <$> many (satisfy (/='\''))) <* char '\''

    parseBool = ((string "true" >> pure (SBool True)) 
              <|> (string "false" >> pure (SBool False)))
    
    parseInt = SInt <$> digits

    parseColor = choice $ 
      map (\(kw, col) -> string kw *> pure (SColor col))
          [ ("red",     CRed)
          , ("green",   CGreen)
          , ("white",   CWhite)
          , ("yellow",  CYellow)
          , ("blue",    CBlue)
          , ("black",   CBlack)
          , ("magenta", CMagenta)
          , ("cyan",    CCyan)
          ]
    
    parseIdent = SString <$> many1 alphaNum

parseStyleDict :: Parser StyleDict
parseStyleDict = do
  lexoid $ char '{'
  sty <- flip sepBy (lexeme $ char ',') $ do
    ident <- lexeme $ many1 alphaNum
    lexoid $ char ':'
    value <- parseStyleValue
    return (ident, value)
  lexoid $ char '}'
  return (StyleDict sty)

srParse :: Parser a -> Parser (SR a)
srParse p = do
  sr <- many (satisfy (\x -> x == '&' || x == '*'))
  let m = map (\x -> if x == '&' then S else R) sr
  let lft = foldl (.) id m
  (lft . E) <$> p

parseDocNode :: Parser DocNode
parseDocNode = do
  lexoid $ char '('
  ident <- lexeme $ many1 alphaNum
  immediate <- option Nothing $ do
    lexoid $ char ':'
    Just <$> parseStyleValue
  style <- option (StyleDict []) parseStyleDict
  recDoc <- (parseInter <|> parseRecs)
  lexoid $ char ')'
  return $ DocNode ident immediate style recDoc

parseDocNodeSR :: Parser DocNodeSR
parseDocNodeSR = DocNodeSR <$> srParse parseDocNode

parseRecs :: Parser NodeStructure
parseRecs = lexeme $ NSRecs <$> many parseDocNodeSR

parseInter :: Parser NodeStructure
parseInter = do
  NSInter <$> (many1 $ lexeme (try parseStr <|> parseDoc))
    where
      parseStr :: Parser Interp
      parseStr = IString <$> (srParse $ (char '%' *> many (satisfy (/= '%')) <* char '%'))

      parseDoc :: Parser Interp
      parseDoc = do
        void $ char '$'
        i <- digits
        d <- parseDocNodeSR
        return (INode i d)

parseStyle :: String -> Either ParseError DocNodeSR
parseStyle = parseWithEof parseDocNodeSR
