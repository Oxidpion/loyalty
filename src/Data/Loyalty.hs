{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Loyalty where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Aeson.TH
import           Data.Aeson.Types     (camelTo2)
import qualified Data.Map             as Map
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Data.Time            (UTCTime, getCurrentTime)
import           Data.Typeable        (Typeable)
import           Data.UUID.Orphans
import           Data.UUID.Types      (UUID)


type LoyaltyId = UUID

data Loyalty = Loyalty
  { loyaltyAudit     :: LoyaltyAudit
  , loyaltyRules     :: !(Map.Map LoyaltyRuleId LoyaltyRule)
  , loyaltyUpdatedAt :: UTCTime
  }

data LoyaltyAudit
  = AuditMin
  | AuditMax
  | AuditSum
  deriving (Show, Read, Eq, Ord)

type LoyaltyRuleId = UUID

data LoyaltyRule = LoyaltyRule
  { loyaltyRuleUpdatedAt  :: UTCTime
  , loyaltyRuleConditions :: [LoyaltyRuleCondition]
  , loyaltyRuleReward     :: LoyaltyRuleReward
  }

data LoyaltyRuleCondition
  = ShopCondition [String]
  | MerchandiseCondition [String]
  | TotalAmountCondition Integer

data LoyaltyRuleReward
  = PercentReward Float
  | FixedReward Integer

$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 7} ''Loyalty)
$(deriveJSON defaultOptions {sumEncoding = defaultTaggedObject {tagFieldName = "type"}} ''LoyaltyRuleCondition)
$(deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 11} ''LoyaltyRule)
$(deriveJSON
    defaultOptions
      {fieldLabelModifier = camelTo2 '_' . drop 17, sumEncoding = defaultTaggedObject {tagFieldName = "type"}}
    ''LoyaltyRuleReward)
$(deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''LoyaltyAudit)


--- Storage description

data LoyaltyStorage = LoyaltyStorage !(Map.Map LoyaltyId Loyalty)

$(deriveSafeCopy 0 'base ''LoyaltyRuleCondition)
$(deriveSafeCopy 0 'base ''LoyaltyRuleReward)
$(deriveSafeCopy 0 'base ''LoyaltyRule)
$(deriveSafeCopy 0 'base ''LoyaltyAudit)
$(deriveSafeCopy 0 'base ''Loyalty)
$(deriveSafeCopy 0 'base ''LoyaltyStorage)


upsertLoyalty :: LoyaltyId -> Loyalty -> Update LoyaltyStorage ()
upsertLoyalty loyId loy
    = do LoyaltyStorage m <- get
         put (LoyaltyStorage (Map.insert loyId loy m))

lookupLoyalty :: LoyaltyId -> Query LoyaltyStorage (Maybe Loyalty)
lookupLoyalty key
    = do LoyaltyStorage m <- ask
         return (Map.lookup key m)

$(makeAcidic ''LoyaltyStorage ['upsertLoyalty, 'lookupLoyalty])

--- Create
newLoyalty :: LoyaltyAudit -> IO Loyalty
newLoyalty audit = do
  currentTime <- getCurrentTime
  return Loyalty { loyaltyAudit = audit, loyaltyRules = Map.empty, loyaltyUpdatedAt = currentTime }

newLoyaltyRule :: [LoyaltyRuleCondition] -> LoyaltyRuleReward -> IO LoyaltyRule
newLoyaltyRule conditions reward = do
  currentTime <- getCurrentTime
  return LoyaltyRule
    { loyaltyRuleUpdatedAt = currentTime
    , loyaltyRuleConditions = conditions
    , loyaltyRuleReward = reward
    }
