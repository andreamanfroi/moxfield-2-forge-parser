module UI where

import Types (Deck(..), commander)
import qualified Data.Text as T
import Converter (convertToForge)
import Parser (parseCommanderDeck, generateFilename)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Data.List (isSuffixOf)

runApp :: IO ()
runApp = do
    putStrLn "Enter input folder path:"
    inputPath <- getLine
    putStrLn "Enter output folder path:"
    outputPath <- getLine

    files <- listDirectory inputPath
    print files
    let moxfieldFiles = filter (".txt" `isSuffixOf`) files

    mapM_ (processFile inputPath outputPath) moxfieldFiles
    putStrLn "Conversion complete!"

processFile :: FilePath -> FilePath -> FilePath -> IO ()
processFile inputPath outputPath file = do
    deck <- parseCommanderDeck (inputPath </> file)
    let forgeDeck = convertToForge deck
        outputFile = outputPath </> generateFilename (commander deck)
    writeFile outputFile (T.unpack forgeDeck)
