{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( LoyaltyId
  , Loyalty
  , LoyaltyApi
  , api
  ) where

import           Data.Api
import           Servant

-- Сервис лояльности ничего не знает о начислениях и списаниях
-- Умеет только расчитывать количество средств, на основе правил и входной покупки
-- Сущность loyalty предстваляет собой набором настроек, в котором указаны правила расчета и
-- способы решения конфликтов между правилами расчета, например
-- максимальный расчет среди правил, минимальный расчет среди правил или суммарный расчет по правилам и т.п
--type LoyaltyApi
---- Получение настроек по расчетам
--   = "loyalty" :> Capture "loyalty_id" LoyaltyId :> Get '[JSON] Loyalty
---- Создание или обновление общих настроек по расчетам
--   :<|> "loyalty" :> Capture "loyalty_id" LoyaltyId :> ReqBody '[JSON] LoyaltySettingRequest :> PutNoContent '[JSON] NoContent
--  -- Добавляет или обновляет новое правило в набор
--   :<|> "loyalty" :> Capture "loyalty_id" LoyaltyId :> "rule" :> Capture "rule_id" LoyaltyRuleId :> ReqBody '[JSON] LoyaltyRule :> PutNoContent '[JSON] NoContent
type LoyaltyApi
   = "loyalty" :> Capture "loyalty_id" LoyaltyId :>
   (    Get '[JSON] Loyalty
   :<|> ReqBody '[JSON] LoyaltySettingRequest :> PutNoContent '[JSON] NoContent
   :<|> "rule" :> Capture "rule_id" LoyaltyRuleId :> ReqBody '[JSON] LoyaltyRuleRequest :> PutNoContent '[JSON] NoContent
   )

api :: Proxy LoyaltyApi
api = Proxy
