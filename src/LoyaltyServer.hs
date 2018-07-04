{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module LoyaltyServer where

import           Api
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader
import qualified Data.Api                   as Api
import           Data.Loyalty
import           Data.Purchase
import           Data.Time                  (UTCTime, getCurrentTime)

import           Data.Acid                  (AcidState, openLocalState)
import           Data.Acid.Advanced         (query', update')
import qualified Data.Map                   as Map
import           Data.UUID.V4               (nextRandom)
import           Servant


serveLoyalty :: AcidState LoyaltyStorage -> Server LoyaltyApi
serveLoyalty st loyId =
       getLoyaltyH st loyId
  :<|> upsertLoyaltyH st loyId
  :<|> upsertLoyaltyRuleH st loyId
  :<|> calculatePurchase st loyId

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

calculatePurchase :: AcidState LoyaltyStorage -> LoyaltyId -> Purchase -> Handler Api.CalculateResponse
calculatePurchase acid loyId purchase = do
    loyalty <- getLoyaltyH acid loyId
    let rules = Map.filter condRules $ loyaltyRules loyalty
    let calcDetail = Map.mapWithKey Api.newCalculateDetail $ Map.map (calcReward . loyaltyRuleReward) rules
    return $ Api.newCalculateResponse $ Map.elems calcDetail
      where
        condRules :: LoyaltyRule -> Bool
        condRules rule = all cond $ loyaltyRuleConditions rule

        cond :: LoyaltyRuleCondition -> Bool
        cond (ShopCondition shops) = elem (purchaseShop purchase) shops
        cond (MerchandiseCondition merchCodes) = any (flip elem merchCodes) $ map merchandiseCode (purchaseMerchandises purchase)
        cond (TotalAmountCondition amount) = amount < amountPurchase

        amountPurchase :: Integer
        amountPurchase = sum (map merchandiseAmount . purchaseMerchandises $ purchase)

        calcReward :: LoyaltyRuleReward -> Integer
        calcReward (PercentReward percent) = round (realToFrac amountPurchase * percent / 100)
        calcReward (FixedReward fix) = fix