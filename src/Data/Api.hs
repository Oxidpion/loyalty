{-# LANGUAGE TemplateHaskell #-}

module Data.Api
  ( LoyaltyId
  , LoyaltyRuleId
  , Loyalty
  , LoyaltySettingRequest(..)
  , LoyaltyRuleRequest(..)
  , CalculateResponse(..)
  , CalculateDetailResponse(..)
  , Purchase
  , newCalculateResponse
  , newCalculateDetail
  ) where

import           Data.Aeson.TH
import           Data.Aeson.Types (camelTo2)
import           Data.Loyalty
import           Data.Purchase

data LoyaltySettingRequest = LoyaltySettingRequest
  { loyaltySettingAuditType :: LoyaltyAudit
  }

data LoyaltyRuleRequest = LoyaltyRuleRequest
  { loyaltyRuleConditions :: [LoyaltyRuleCondition]
  , loyaltyRuleReward     :: LoyaltyRuleReward
  }

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14} ''LoyaltySettingRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 11} ''LoyaltyRuleRequest)

data CalculateResponse = CalculateResponse
  { calculateAmount  :: Integer
  , calculateDetails :: [CalculateDetailResponse]
  }

data CalculateDetailResponse = CalculateDetailResponse
  { calculateDetailRuleId      :: LoyaltyRuleId
  , calculateDetailAmount      :: Integer
  }

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 15} ''CalculateDetailResponse)
$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 9} ''CalculateResponse)

newCalculateResponse :: [CalculateDetailResponse] -> CalculateResponse
newCalculateResponse details = CalculateResponse
  { calculateAmount = reward
  , calculateDetails = details
  } where reward = sum . map calculateDetailAmount $ details

newCalculateDetail :: LoyaltyRuleId -> Integer -> CalculateDetailResponse
newCalculateDetail = CalculateDetailResponse
