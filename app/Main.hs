module Main where

import Lib (parseRows)
import qualified Text.Trifecta as Tri
import Text.Show.Pretty (pPrint, ppShow)
import Options.Applicative
import Data.Semigroup ((<>))


data Source = FilePath String 
             | StdIn
             | Bare
             deriving (Eq, Show)


filePath :: Parser Source
filePath =
  FilePath <$>
  option str (metavar "FILE" <> long "input" <> short 'i' <> help "Parse from file.")

-- | Always succeeds.
stdIn :: Parser Source
stdIn =
  const StdIn <$>
  flag'  StdIn (long "stdin" <> short 's' <> help "Parse from stdin.")

dispatch :: Source -> IO ()
dispatch source = do
  let displayTally = pPrint . parseRows ["a", "k", "l"]
  case source of
    (FilePath fp) -> readFile fp >>= displayTally
    StdIn -> getContents >>= displayTally
    Bare -> putStrLn version

toplevel :: Parser Source
toplevel =
  stdIn <|> filePath <|>
  (infoOption version (long "version" <> short 'V') <*> pure Bare) <|> pure Bare -- VERSION

-- VERSION
version = "kitty 0.0.1"

main :: IO ()
main = execParser opts >>= dispatch 
  where headerLn = "kitty -- Parse & summarize kitty csv files."
        opts  = info (helper <*> toplevel) (fullDesc <> header headerLn)
