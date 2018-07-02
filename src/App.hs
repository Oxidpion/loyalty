module App
  ( run
  ) where

import           Api
import           Data.Acid
import           Data.Loyalty
import qualified Data.Map                 as Map
import           LoyaltyServer
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

run :: IO ()
run = Warp.run 1234 =<< mkApp

app :: AcidState LoyaltyStorage -> Application
app = serve api . serveLoyalty

mkApp :: IO Application
mkApp = do
  acidLoyalty <- openLocalState $ LoyaltyStorage Map.empty
  return $ app acidLoyalty
