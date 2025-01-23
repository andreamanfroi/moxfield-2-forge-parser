module Types
    ( Card(..)
    , Deck(..)
    ) where

import Data.Text (Text)

-- Represent a single card
data Card = Card
    { name  :: Text -- Card name
    , count :: Int  -- Quantity
    } deriving (Show, Eq)

-- Represent a Commander deck
data Deck = Deck
    { title     :: Text        -- Deck title
    , mainDeck  :: [Card]      -- Main deck cards
    , sideboard :: [Card]      -- Sideboard cards
    , commander :: Card        -- Commander card
    } deriving (Show, Eq)
