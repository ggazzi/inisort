module Main where

import           Control.Applicative
import           Data.Attoparsec.Text (parseOnly)
import qualified Data.Char            as Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Ini             (Ini (..), keyValueParser, parseIni)
import qualified Data.List            as List
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text
import           Options.Applicative
import           Prelude              hiding (takeWhile)
import           System.IO            (IOMode (..), stdin, stdout, withFile)

import           Data.Ini.Line



data Options = Options
  { referenceFile :: FilePath
  , unsortedFile  :: Maybe FilePath
  , outputFile    :: Maybe FilePath
  }

opts :: Parser Options
opts = Options
  <$> strArgument
        ( metavar "REFERENCE_FILE"
       <> help "File in the desired order" )
  <*> (optional . strArgument)
        ( metavar "UNSORTED_FILE"
       <> help "File with the desired entries, but unsorted. By default read from stdin." )
  <*> (optional . strOption)
       ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Write output to FILE, by default write to stdout" )

main :: IO ()
main = do
  options <- execParser $ info (opts <**> helper)
     ( fullDesc
    <> progDesc "Sort an INI file according to the entries of another INI file." )
  run (referenceFile options) (unsortedFile options)
    (outputFile options)


run referenceFile unsortedFile outputFile = do
  referenceResult <- parseOnly iniLinesParser <$> Text.readFile referenceFile
  unsortedText <- case unsortedFile of
    Nothing   -> Text.getContents
    Just file -> Text.readFile file
  let unsortedResult = parseIni unsortedText

  case (unsortedResult, referenceResult) of
    (Left err, _) -> putStrLn $ "Error parsing '" ++ fromMaybe "<stdin>" unsortedFile ++ "': " ++ err
    (_, Left err) -> putStrLn $ "Error parsing '" ++ referenceFile ++ "': " ++ err
    (Right (Ini unsortedEntries), Right referenceLines) ->
      let sortedEntries = sortEntriesAs referenceLines unsortedEntries
      in withOutputFile $ \writeLine ->
        mapM_ (writeLine . showIniLine) sortedEntries
  where
    withOutputFile action = case outputFile of
        Nothing -> action Text.putStrLn
        Just file -> withFile file WriteMode $ \out -> action (Text.hPutStrLn out)
