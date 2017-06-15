{-# LANGUAGE OverloadedStrings #-}
module Data.Ini.Line where

import           Control.Applicative
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import qualified Data.Char                  as Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Ini                   (Ini (..), keyValueParser,
                                             readIniFile)
import qualified Data.List                  as List
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Prelude                    hiding (takeWhile)


data IniLine
  = Section Text
  | Comment Text
  | Entry Text Text
  | Blank
  deriving (Eq, Show)

showIniLine :: IniLine -> Text
showIniLine (Section name)    = "[" <> name <> "]"
showIniLine (Comment text)    = "#" <> text
showIniLine (Entry key value) = key <> "=" <> value
showIniLine Blank             = ""


iniLinesParser =
  many' iniLineParser <* endOfInput

iniLineParser = section <|> comment <|> entryOrBlank <?> "INI line"
  where
    section = do
      _ <- char '['
      name <- takeWhile1 (\c -> c /= ']' && c /= '[')
      _ <- char ']'
      skipEndOfLine
      return (Section name)

    comment = do
      _ <- satisfy (\c -> c == ';' || c == '#')
      text <- takeWhile (not . isEndOfLine)
      endOfLine
      return (Comment text)

    entryOrBlank = do
      key <- takeWhile (\c -> not (c == '=' || c == ':' || c == '[' || c == ']' || isEndOfLine c))
      if Text.null key
        then blank
        else entry key <|> blank

    entry key = do
      _ <- char '=' <|> (char ':' *> char ' ')
      value <- takeWhile (not . isEndOfLine)
      endOfLine
      return $ Entry (Text.strip key) (Text.strip value)

    blank = endOfLine *> return Blank

    skipEndOfLine =
      skipWhile (\c -> Char.isSpace c && not (isEndOfLine c))
        <* satisfy isEndOfLine


sortEntriesAs :: [IniLine] -> HashMap Text (HashMap Text Text) -> [IniLine]
sortEntriesAs lines sections =
    findFirstSection lines
  where
    findFirstSection []                    = missingSections sections
    findFirstSection (Section name : rest) = beginSection sections name rest
    findFirstSection (line : rest)         = line : findFirstSection rest

    findNextSection sections []             = missingSections sections
    findNextSection _ (Section name : rest) = beginSection sections name rest
    findNextSection _ (_ : rest)            = findNextSection sections rest

    beginSection sections sectionName lines =
      case HashMap.lookup sectionName sections of
        Just entries ->
          Section sectionName : sortSection (HashMap.delete sectionName sections) entries lines
        Nothing      -> findNextSection sections lines

    sortSection sections entries [] = missingEntries entries ++ missingSections sections
    sortSection sections entries (Entry key _ : rest) =
      case HashMap.lookup key entries of
        Just value -> Entry key value : sortSection sections (HashMap.delete key entries) rest
        Nothing -> sortSection sections entries rest
    sortSection sections entries (Section name : rest) =
      missingEntries entries ++ beginSection sections name rest
    sortSection sections entries (line : rest) = line : sortSection sections entries rest

    missingEntries =
      (Blank:) . (++[Blank]) . map (uncurry Entry) . HashMap.toList

    missingSections =
      concatMap missingSection . HashMap.toList

    missingSection (name, entries) =
      Blank : Section name : map (uncurry Entry) (HashMap.toList entries) ++ [Blank]
