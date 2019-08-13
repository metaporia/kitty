{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Lib hiding (header)
import Options.Applicative
import Text.Show.Pretty (pPrint)
import qualified Text.Trifecta as Tri

-- TODO
-- □  read in header file with ids
-- □  apply debt reduction (if convenient)
data Source
  = FilePath String
  | StdIn
  | Bare
  deriving (Eq, Show)

filePath :: Parser Source
filePath =
  FilePath <$>
  option
    str
    (metavar "FILE" <> long "input" <> short 'i' <> help "Parse from file.")

-- | Always succeeds.
stdIn :: Parser Source
stdIn =
  StdIn <$ flag' StdIn (long "stdin" <> short 's' <> help "Parse from stdin.")

header' :: Parser Ids
header' = headerArg <|> headerFile

headerArg :: Parser Ids
headerArg =
  ByArgument <$>
  strOption
    (metavar "COMMA_SEP_IDS" <> long "names" <> short 'n' <>
     help "Read in comma separated list of identifiers.")

headerFile :: Parser Ids
headerFile =
  FromFile <$>
  strOption
    (metavar "FILEPATH" <> long "names-file" <> short 'f' <>
     help "Read in comma separated list of identifiers from file.")

data Ids
  = ByArgument String
  | FromFile String
  deriving (Eq, Show)

-- | Maps identifiers to the amount owed
type DebtLog = Map String Float

-- | Each row represents a shared purchase or transaction.
--
-- We want to know how much each party owes each other party.
--
-- The debt between two parties is stored at the (i,j)th cell of the debt
-- matrix, which i and j are the associated indices of the two parties. The
-- sign of the debt will be the amount i owes j, so when one wants to know how
-- much j owes i, simply negate the result of the more primitive request.
--
processRow :: Row -> Debts -> IO ()
processRow Row {..} _ = undefined

-- | A.t.m. only produces tabluated form of csv.
dispatch :: (Source, Ids) -> IO ()
dispatch (source, ids) = do
  let idRes =
        case ids of
          ByArgument header -> return $ parseHeader header
          FromFile fp -> parseHeader <$> readFile fp
  let tabulate ids content =
        case parseRows ids content of
          Tri.Success rows ->
            case fmap asPayments $
                 flip applyPayments (blankDebts ids) $
                 rows >>= (generatePaymentEdgesForRow . debtors ids) of
              Just payments ->
                traverse_
                  (\(i, j, debt) ->
                     putStrLn $ i ++ " owes " ++ j ++ ": $" ++ show debt)
                  payments
              Nothing ->
                putStrLn "Malformed id lookup table or transaction matrix."
          Tri.Failure err -> pPrint err
  -- TODO factor out parse and render logic
  case source of
    (FilePath fp) -> do
      contents <- readFile fp
      idRes' <- idRes
      case idRes' of
        Tri.Failure err -> putStrLn $ "Unable to parse header: \n" ++ show err
        Tri.Success ids -> tabulate ids contents
    StdIn -> do
      contents <- getContents
      idRes' <- idRes
      case idRes' of
        Tri.Failure err -> putStrLn $ "Unable to parse header: \n" ++ show err
        Tri.Success ids -> tabulate ids contents
    Bare -> putStrLn version

toplevel :: Parser (Source, Ids)
toplevel =
  infoOption version (long "version" <> short 'V') <*>
  ((,) <$> (stdIn <|> filePath <|> pure Bare) <*> header')

-- VERSION
version :: String
version = "kitty 0.0.2"

main :: IO ()
main = execParser opts >>= dispatch
  where
    headerLn = "kitty -- Parse & summarize kitty csv files."
    opts = info (helper <*> toplevel) (fullDesc <> header headerLn)
