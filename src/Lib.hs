{-# LANGUAGE OverloadedStrings, RecordWildCards,
  ScopedTypeVariables, QuasiQuotes #-}

module Lib (rows, parseRows) where

import Control.Applicative
import Control.Monad
import Data.Foldable (asum)
import Text.Parser.Combinators
import Text.RawString.QQ
import Text.Read (readEither)
import Text.Show.Pretty (pPrint, ppShow)
import qualified Text.Trifecta as Tri
import Text.Trifecta

-- <dollars-spent>,<participated>,<paid>
exCsv =
  [r|30,kl,k
120,akl,l
35,al,a
300,l,k
|]

exRow = "120,akl,l"

price :: Parser (Either String Float)
price = do
  readEither <$> (some digit <?> "price expected to be only digits")

-- | The input list should consist of unique alphanumeric identifiers.
identifier ::
     [String] -- | ids
  -> Parser String
identifier
  -- FIXME assert uniqueness
  --xs <- oneOf (string <$> ids)
 = do
  choice . fmap (\i -> string i <?> "unexpected identifier: \n" ++ i)

ourIdentifier :: Parser String
ourIdentifier = identifier ["a", "k", "l"]

ourIds = ["a", "k", "l"]

ourRow = row ["a", "k", "l"]

data Row = Row
  { dollars :: Float
  , participants :: [String]
  , payers :: [String]
  } deriving (Eq, Show)

row :: [String] -> Parser Row
row ids = do
  let collectIds = some $ identifier ids
      sep = char ',' <* skipOptional space
  eitherDollars <- price
  dollars <-
    case eitherDollars of
      Left err -> fail (ppShow err)
      Right dollars -> return dollars
  _ <- sep
  participants <- collectIds
  _ <- sep
  payers <- collectIds
  return $ Row {..}

-- | Parse kitty csv file.
rows :: [String] -> Parser [Row]
rows ids = (row ids  `sepEndBy` newline) <* eof

parseRows :: [String] -> String -> Result [Row]
parseRows ids = test (rows ids) 

ourRows = rows ourIds

test :: Parser a -> String -> Result a
test parser = parseString parser mempty
