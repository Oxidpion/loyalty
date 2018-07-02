{-# LANGUAGE TemplateHaskell #-}

module Data.Api
  ( LoyaltyId
  , LoyaltyRuleId
  , Loyalty
  , LoyaltySettingRequest(..)
  , LoyaltyRuleRequest(..)
  ) where

import           Data.Aeson.TH
import           Data.Aeson.Types (camelTo2)
import           Data.Loyalty

data LoyaltySettingRequest = LoyaltySettingRequest
  { loyaltySettingAuditType :: LoyaltyAudit
  }

data LoyaltyRuleRequest = LoyaltyRuleRequest
  { loyaltyRuleConditions :: [LoyaltyRuleCondition]
  , loyaltyRuleReward     :: LoyaltyRuleReward
  }

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14} ''LoyaltySettingRequest)
$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 11} ''LoyaltyRuleRequest)