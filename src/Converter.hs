module Converter where

import Types
import Data.Text (Text, pack)
import qualified Data.Text as T

convertToForge :: Deck -> Text
convertToForge deck =
    let metadata = pack "[metadata]\nName=" <> name (commander deck) <> pack "_centurion_vers1"
        formatCommander = pack "[commander]\n" <> pack (show (count (commander deck))) <> pack " " <> name (commander deck)
        avatar = pack "[Avatar]\n"             
        mainHeader = pack "[Main]"            
        sideboardHeader = pack "[Sideboard]"    
        extras = pack "[Planes]\n\n[Schemes]\n\n[Conspiracy]\n" 
        mainDeckLines = map formatCard (mainDeck deck)
        sideboardLines = map formatCard (sideboard deck)
    in T.unlines $ [metadata, formatCommander, avatar, mainHeader] ++ mainDeckLines ++ [sideboardHeader] ++ sideboardLines ++ [extras]

formatCard :: Card -> Text
formatCard (Card name count) = T.pack (show count) <> pack " " <> name
