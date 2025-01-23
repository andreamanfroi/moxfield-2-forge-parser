module Parser where

import Types
import Data.Text (Text, pack, strip)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Data.Maybe (mapMaybe)
import System.FilePath (takeBaseName)

-- Parse a Commander deck from a file
parseCommanderDeck :: FilePath -> IO Deck
parseCommanderDeck filePath = do
    content <- TIO.readFile filePath
    let deckLines = T.lines content
        title = sanitizeDeckName (pack (takeBaseName filePath))
        (mainLines, sideboardLines) = splitDeckSections deckLines
        (commanderLine, mainDeckLines) = extractCommander mainLines
        mainDeck = mapMaybe parseCard mainDeckLines
        sideboard = mapMaybe parseCard sideboardLines
        commander = parseCommander commanderLine
    return $ Deck title mainDeck sideboard commander

-- Extract the Commander and remaining main deck lines
extractCommander :: [Text] -> (Text, [Text])
extractCommander lines =
    let commanderLine = last lines
        mainDeckLines = init lines
    in (commanderLine, mainDeckLines)

-- Split the deck into main and sideboard sections
splitDeckSections :: [Text] -> ([Text], [Text])
splitDeckSections lines =
    let (main, rest) = break (== pack "SIDEBOARD:") lines
    in (main, drop 1 rest) -- Drop the "SIDEBOARD:" header

-- Parse a card line into a Card
parseCard :: Text -> Maybe Card
parseCard line =
    let (countText, nameText) = T.breakOn (pack " ") (strip line)
    in case decimal countText of
        Right (count, _) -> Just $ Card (strip nameText) count
        Left _ -> Nothing -- If parsing fails, return Nothing

-- Parse the Commander card (required)
parseCommander :: Text -> Card
parseCommander line =
    case parseCard line of
        Just card -> card
        Nothing -> error $ "Invalid Commander card: " ++ T.unpack line

-- Sanitize the deck name
sanitizeDeckName :: Text -> Text
sanitizeDeckName name =
    let validPart = T.takeWhile (/= '-') name
    in T.replace (pack " ") (pack "_") (strip validPart)

-- Generate the filename based on the Commander name
generateFilename :: Card -> FilePath
generateFilename (Card commanderName _) =
    let sanitized = T.replace (pack " ") (pack "") commanderName -- Remove spaces only
    in T.unpack sanitized ++ "_centurion_vers1.dck"