-------------------------------------------------------------------------------
-- |
-- Module      :  Data.URI.QueryString
-- Copyright   :  (c) Naushadh 2018
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  naushadh@protonmail.com
--
-- A querystring parser with nesting support.
--
-------------------------------------------------------------------------------

module Data.URI.QueryString (
    QueryString
  , Param
  , ParamKey(..)
  , ParamVal(..)
  , Parser
  , runParser
  , parseQueryString
) where

import qualified Text.ParserCombinators.ReadP as P
import qualified Data.List as List
import           Control.Applicative ((<|>))
import           Data.Char (chr)
import           Numeric (readHex)
import           Data.Monoid ((<>))

-------------------------------------------------------------------------------
-- * Types

-- | The entire QueryString parsed into a map like structure.
type QueryString = [Param]

-- | A single parameter in a 'QueryString'
type Param = (ParamKey, ParamVal)

-- | Parameter key for a 'Param'
-- TODO: add smart constructor to prevent illegal characters
newtype ParamKey = ParamKey { unParamKey :: String }
  deriving (Show, Eq)

-- | Parameter value for a 'Param'
-- TODO: add smart constructor to prevent illegal characters
data ParamVal = PVLeaf String     -- ^ x=val
              | PVList [ParamVal] -- ^ x[]=val1&x[]=val2
              | PVNode [Param]    -- ^ x[key1]=val1&x[key2]=val2
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- * Parsers

-- | Generic parser type.
type Parser a = P.ReadP a

-- | run a 'Parser'. returns Just when parsing was OK.
-- TODO: replace this with something that can tell what went wrong.
runParser :: Parser a -> String -> Maybe a
runParser p s = case null results of
  True  -> Nothing
  False -> pure . fst . last $ results
  where
    results = (P.readP_to_S p) s

-- | Parse a rfc3986 compliant query string into a map-like 'QueryString'.
-- TODO: verify various malformed input scenarios.
parseQueryString :: Parser QueryString
parseQueryString = condense . fmap i2e <$> parseIQueryString

oneOf :: (a -> Parser a) -> [a] -> Parser a
oneOf p xs = P.choice (p <$> xs)

letter :: Parser Char
letter =  oneOf P.char ['A'..'Z']
      <|> oneOf P.char ['a'..'z']

digit :: Parser Char
digit = oneOf P.char ['0'..'9']

-- percentEncode :: Char -> String
-- percentEncode = flip showHex "" . ord

percentDecode :: Parser Char
percentDecode = chr <$> h
  where
    l = (*10) <$> P.readS_to_P readHex
    r = P.readS_to_P readHex
    h = (+) <$> l <*> r

-------------------------------------------------------------------------------
-- * CHAR PARSERS

-- | Percent-sign prefixed encoded characters that when decoded can violate
-- rules. (i.e., reserved characters used in an invalid place).
-- <https://tools.ietf.org/html/rfc3986#section-2.1 Percent-Encoding>
parsePercentEncoded :: Parser Char
parsePercentEncoded = P.char '%' *> percentDecode

-- | Unreserved characters.
-- <https://tools.ietf.org/html/rfc3986#section-1.1.2 Unreserved Characters>
parseUnreserved :: Parser Char
parseUnreserved
  = P.choice [ letter, digit, P.char '/', P.char '.', P.char '_', P.char '~']

-- -- | A subset of the reserved characters (sub-delims) is used as delimiters
-- -- witihin a generic URI component.
-- -- <https://tools.ietf.org/html/rfc3986#section-2.2 Reserved Characters>
-- subDelimiters :: [Char]
-- subDelimiters = ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

-- -- | Parse characters in a Path.
-- -- <https://tools.ietf.org/html/rfc3986#section-3.3 Path Character>
-- parsePathChar :: Parser Char
-- parsePathChar
--   = P.choice  [ parseUnreserved, parsePercentEncoded
--               , oneOf P.char subDelimiters, P.char ':', P.char '@'
--               ]

-- -- | Parse characters in a Query.
-- -- <https://tools.ietf.org/html/rfc3986#section-3.4 Query>
-- parseQueryChar :: Parser Char
-- parseQueryChar
--   =  parsePathChar
--  <|> P.char '/'
--  <|> P.char '?'

-- | Parse an character used either as a param key or value.
-- like 'parseQueryChar' but excluding delimiters
parseParamChar :: Parser Char
parseParamChar
  = P.choice  [ parseUnreserved, parsePercentEncoded, P.char ':', P.char '@'
              , P.char '/', P.char '?'
              ]

-------------------------------------------------------------------------------
-- * INTERNAL AST + PARSERS

-- | A parameter key with several nested keys.
-- ex: key[nestedKey1][nestedKey2]...[nestedKeyN]
data IKey = IKey String [String]
  deriving Show

-- renderIKey :: IKey -> String
-- renderIKey (IKey s []) = s
-- renderIKey (IKey s ns) = s <> mconcat (go <$> ns)
--   where go n = "[" <> n <> "]"

parseIKey :: Parser IKey
parseIKey
  =  IKey
 <$> P.many1 parseParamChar
 <*> P.many (P.char '[' *> parseInnerKey <* P.char ']')
  where
    parseInnerKey = P.many parseParamChar

-- | A parameter value.
-- "=foo" = IVal "foo"
-- "=" = IVal ""
-- "" = IVal ""
newtype IVal = IVal String
  deriving Show

-- renderIVal :: IVal -> String
-- renderIVal (IVal "") = ""
-- renderIVal (IVal v) = "=" <> v

parseIVal :: Parser IVal
parseIVal = P.optional (P.char '=') *> (IVal <$> P.many parseParamChar)

-- | A Parameter with key and value
type IParam = (IKey, IVal)

-- renderIParam :: IParam -> String
-- renderIParam (k,v) = renderIKey k <> renderIVal v

parseIParam :: Parser IParam
parseIParam = (,) <$> parseIKey <*> parseIVal

-- | The entire QueryString
type IQueryString = [IParam]

-- renderIQueryString :: IQueryString -> String
-- renderIQueryString ps = List.intercalate "&" $ renderIParam <$> ps

parseIQueryString :: Parser IQueryString
parseIQueryString = parseIParam `P.sepBy1` (P.char '&')

-------------------------------------------------------------------------------
-- * INTERNAL -> EXTERNAL

i2e :: IParam -> Param
i2e (IKey ik nks, IVal iv) = mkParam nks (PVLeaf iv)
  where
    mkParam :: [String] -> ParamVal -> Param
    mkParam [] v = (ParamKey ik, v)
    mkParam ks v = case last ks of
      "" -> mkParam (init ks) (PVList [v])
      k  -> mkParam (init ks) (PVNode $ [(ParamKey k, v)])

condense :: QueryString -> QueryString
condense = List.foldl' (flip go) []
  where
    lookupFull :: ParamKey -> QueryString -> Maybe Param
    lookupFull k ps = (,) <$> pure k <*> lookup k ps
    go :: Param -> QueryString -> QueryString
    go p@(k,PVList vs) ps = case lookupFull k ps of
      Just p0@(_, PVList vs0) -> go (k, PVList $ vs0 <> vs) (List.delete p0 ps)
      Just _ -> p:ps
      Nothing -> p:ps
    go p@(k,PVNode vs) ps = case lookupFull k ps of
      Just p0@(_, PVNode vs0) -> go (k, PVNode $ vs0 <> vs) (List.delete p0 ps)
      Just _ -> p:ps
      Nothing -> p:ps
    go p ps = p:ps
