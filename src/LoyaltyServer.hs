{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module LoyaltyServer where

import           Api
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader
import qualified Data.Api                   as Api
import           Data.Loyalty
import           Data.Time                  (UTCTime, getCurrentTime)

import           Data.Acid                  (AcidState, openLocalState)
import           Data.Acid.Advanced         (update', query')
import qualified Data.Map                   as Map
import           Data.UUID.V4               (nextRandom)
import           Servant


serveLoyalty :: AcidState LoyaltyStorage -> Server LoyaltyApi
serveLoyalty st loyId =
       getLoyaltyH st loyId
  :<|> upsertLoyaltyH st loyId
  :<|> upsertLoyaltyRuleH st loyId

getLoyaltyH :: AcidState LoyaltyStorage -> LoyaltyId -> Handler Loyalty
getLoyaltyH acid loyId = do
    maybeLoyalty <- query' acid $ LookupLoyalty loyId
    maybe (throwError $ err404 { errBody = "Loyalty not found" }) return maybeLoyalty

upsertLoyaltyH :: AcidState LoyaltyStorage -> LoyaltyId -> Api.LoyaltySettingRequest -> Handler NoContent
upsertLoyaltyH acid loyId setting = do
    loyalty <- liftIO $ newLoyalty $ Api.loyaltySettingAuditType setting
    update' acid $ UpsertLoyalty loyId loyalty
    return NoContent

upsertLoyaltyRuleH :: AcidState LoyaltyStorage -> LoyaltyId -> LoyaltyRuleId -> Api.LoyaltyRuleRequest -> Handler NoContent
upsertLoyaltyRuleH acid loyId ruleId rule = do
    loyalty <- getLoyaltyH acid loyId
    loyaltyRule <- liftIO $ newLoyaltyRule (Api.loyaltyRuleConditions rule) (Api.loyaltyRuleReward rule)
    let loyaltyUpd = loyalty { loyaltyRules = Map.insert ruleId loyaltyRule (loyaltyRules loyalty) }
    update' acid $ UpsertLoyalty loyId loyaltyUpd
    return NoContent
