{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes
  #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Applicative
import Control.Monad
import Data.Bool (bool)
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix hiding ((<|>), trace)
import Data.Maybe (catMaybes)
import Helpers
import Text.Parser.Combinators
import qualified Text.PrettyPrint.Boxes as Box
import Text.PrettyPrint.Boxes ((/+/), (<+>))
import Text.RawString.QQ
import Text.Read (readEither)
import Text.Show.Pretty (ppShow)
import Text.Trifecta as Tri hiding (err)

--- | PARSE
parse :: a
parse = undefined

-- <dollars-spent>,<participated>,<paid>
exCsv :: String
exCsv =
  [r|30,kl,k
120,akl,l
35,al,a
300,l,k
|]

-- a owes l 40
-- k owes l 40
exRow :: String
exRow = "120,akl,l"

price :: Parser (Either String Float)
price = readEither <$> (some digit <?> "price expected to be only digits")

-- | The input list should consist of unique alphanumeric identifiers.
identifier ::
     [String] -- | ids
  -> Parser String
identifier
  -- FIXME assert uniqueness
  --xs <- oneOf (string <$> ids)
 = choice . fmap (\i -> string i <?> "unexpected identifier: \n" ++ i)

ourIdentifier :: Parser String
ourIdentifier = identifier ["a", "k", "l"]

ourIds :: [String]
ourIds = ["a", "k", "l"]

ourRow :: Parser Row
ourRow = row ["a", "k", "l"]

data Row = Row
  { dollars :: Float
  , participants :: [String]
  , payers :: [String]
  } deriving (Eq, Show)

header :: Parser [Id]
header = some alphaNum `sepBy` (many space *> char ',' <* many space)

parseHeader :: String -> Result [String]
parseHeader = test header

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
rows ids = (row ids `sepEndBy` newline) <* eof

parseRows :: [String] -> String -> Result [Row]
parseRows ids = test (rows ids)

test :: Parser a -> String -> Result a
test parser = parseString parser mempty

--- | PROCESS
process :: a
process = undefined

-- | Pretty-print rows in tabular format.
-- 
-- Each (unique) identifier should have its own column
showRows :: [Row] -> String
showRows =
  (\(c1, c2, c3) -> Box.render $ c1 <+> c2 <+> c3) .
  foldr
    (\Row {..} (c1, c2, c3) ->
       ( Box.text (show dollars) /+/ c1
       , Box.text (concat participants) /+/ c2
       , Box.text (concat payers) /+/ c3))
    (Box.nullBox, Box.nullBox, Box.nullBox)

printRows :: [Row] -> IO ()
printRows = putStrLn . showRows

-- | Maps identifiers to the amount owed
type DebtLog = Map String Float

-- | The (i,j)th cell contains the amount that party i owes party j.
data Debts = Debts
  { indices :: Map String Int
  , matrix :: Matrix Float
  } deriving (Eq, Show)

-- | Updates cell value if it exists, otherwise returns Nothing.
-- 
updateDebtsAt ::
     Id -- | ith row
  -> Id -- | jth column
  -> (Float -> Float)
  -> Debts
  -> Maybe Debts
updateDebtsAt i j f (Debts indices mat) = do
  ix <- Map.lookup i indices
  jx <- Map.lookup j indices
  debtCell <- safeGet ix jx mat
  return $ Debts indices $ setElem (f debtCell) (ix, jx) mat

-- | Returns nothing on corrupted indices table or incorrectly constructed matrix.
applyPayment :: Payment -> Debts -> Maybe Debts
applyPayment (i, j, debt) = updateDebtsAt i j (+ debt)

applyPayments :: [Payment] -> Debts -> Maybe Debts
applyPayments payments debts =
  foldr
    (\payment mDebts -> mDebts >>= applyPayment payment)
    (Just debts)
    payments

blankDebts ::
     [String] -- | ids
  -> Debts
blankDebts ids =
  let indices = Map.fromList $ zip ids [1 ..]
      matrix = Data.Matrix.matrix n n (const 0)
        where
          n = length ids
  in Debts {..}

-- | Each row represents a shared purchase or transaction.
--
-- We want to know how much each party owes each other party.
--
-- The debt between two parties is stored at the (i,j)th cell of the debt
-- matrix, which i and j are the associated indices of the two parties. The
-- sign of the debt will be the amount i owes j, so when one wants to know how
-- much j owes i, simply negate the result of the more primitive request.
--
processRow :: Row -> Debts -> IO [(String, String, Float)]
processRow Row {..} _ = undefined -- see 'generatePaymentEdgesForRow'

exs :: [String]
exs
  -- l owes k 15
 =
  [ "30,kl,k"
  -- l & k each spent 60
  -- a owes k 20 
  -- a owes l 20
  , "120,akl,lk"
  , "35,al,a"
  , "300,l,k"
  ]

-- | 
-- > exRows = parseRows ourIds exCsv
exRows :: [Row]
exRows =
  [ Row {dollars = 30.0, participants = ["k", "l"], payers = ["k"]}
  , Row {dollars = 120.0, participants = ["a", "k", "l"], payers = ["l"]}
  , Row {dollars = 35.0, participants = ["a", "l"], payers = ["a"]}
  , Row {dollars = 300.0, participants = ["l"], payers = ["k"]}
  ]

data Debtor
  = Owes Id
         Float
  | Owed Id
         Float
  deriving (Eq, Show)

updateDebt :: (Float -> Float) -> Debtor -> Debtor
updateDebt f debtor =
  case debtor of
    Owes id x -> Owes id $ f x
    Owed id x -> Owed id $ f x

getDebt :: Debtor -> Float
getDebt (Owes _ x) = x
getDebt (Owed _ x) = x

getId :: Debtor -> Id
getId (Owes id _) = id
getId (Owed id _) = id

isOwed :: Debtor -> Bool
isOwed (Owed _ _) = True
isOwed _ = False

isOwes :: Debtor -> Bool
isOwes (Owes _ _) = True
isOwes _ = False

type Id = String

-- | Determine whether a given party has overpaid, underpaid, or has paid the
-- correct amount.
assignDebt :: Id -> Row -> Maybe Debtor
assignDebt id Row {dollars, participants, payers}
      -- determine whether party is a participant, a payer, or both
 =
  let (isParticipant, isPayer) = (id `elem` participants, id `elem` payers)
      idealPayment = dollars / fromIntegral (length participants)
      actualPayment = dollars / fromIntegral (length payers)
  in if | isParticipant && isPayer ->
          let diff = actualPayment - idealPayment
          in if | diff == 0 -> Nothing
                | diff < 0 -> Just $ Owes id diff
                | otherwise -- diff > 0 
                 -> Just $ Owed id diff
        | isParticipant -> Just $ Owes id idealPayment
        | isPayer -> Just $ Owed id $ dollars - actualPayment
        | otherwise -> Nothing

-- | Generate all debtors for a given row. Filter out 'PaidUp'.
debtors :: [Id] -> Row -> [Debtor]
debtors ids row = flip assignDebt row `filtermap` ids

type Payment = (Id, Id, Float)

-- | Reduce debtors into debt edges--that is, directed payments between two
-- parties.
--
-- In the first pass we will convert to edges on a per-'Row' basis, collect
-- each 'Row'\'s payment edges, and then reduce the whole ledger's payment
-- edges into a table of payments.
generatePaymentEdgesForRow :: [Debtor] -> [(Id, Id, Float)]
generatePaymentEdgesForRow debtors =
  let (owed, owes) = partition isOwed debtors
      -- assertion: the sum of the owed should equal the sum of the owes.
      go [] _ = []
      go (recipient:t) owers
        -- first pass: left-biased payment
       =
        let amountOwed = getDebt recipient
            (payments, remainingDebtors, _) =
              foldr
                (\debtor (payments, remainingDebtors, amountOwed') ->
                   let amountOwed''
                         -- trace (show amountOwed' ++ " " ++ show (getDebt debtor)) $
                        = amountOwed' - getDebt debtor
                   in if | amountOwed'' == 0 -- payment removes both payer and recipient
                          ->
                           ( (getId debtor, getId recipient, getDebt debtor) :
                             payments
                           , remainingDebtors
                           , 0)
                         | amountOwed'' > 0 -- payment removes payer
                          ->
                           ( (getId debtor, getId recipient, getDebt debtor) :
                             payments
                           , remainingDebtors
                           , amountOwed'')
                         | otherwise -- @amountOwed'' < 0@ payment removes recipient
                          ->
                           ( ( getId debtor
                             , getId recipient
                             , getDebt debtor - amountOwed') :
                             payments
                           , updateDebt (const (negate amountOwed'')) debtor :
                             remainingDebtors
                           , 0))
                ([], [], amountOwed)
                owers
        in payments ++ go t remainingDebtors
  in go owed owes

asPayments :: Debts -> [Payment]
asPayments (Debts indices mat) =
  let keys = Map.keys indices
  in catMaybes
       [ (i, j, ) <$> debt
       | i <- keys
       , j <- keys
       , let debt -- filter out zero entries
              =
               (\case
                  Nothing -> Nothing
                  Just d' -> bool (Just d') Nothing (d' == 0)) $
               join $
               -- convert ids to indices, get cel value
               safeGet <$> Map.lookup i indices <*> Map.lookup j indices <*>
               pure mat
       ]
