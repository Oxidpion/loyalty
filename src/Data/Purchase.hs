{-# LANGUAGE TemplateHaskell #-}

module Data.Purchase where

import           Data.Aeson.TH
import           Data.Aeson.Types (camelTo2)

type Shop = String

data Purchase = Purshase
  { purchaseShop         :: Shop
  , purchaseMerchandises :: [Merchandise]
  }

data Merchandise = Merchandise
  { merchandiseCode     :: String
  , merchandiseQuantity :: Integer
  , merchandiseAmount   :: Integer
  }

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 8} ''Purchase)
$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 11} ''Merchandise)