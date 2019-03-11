{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Language.PlutusTx.Unlift (Unlift, unlift) where

import Ledger
import Language.PlutusTx.Evaluation (evaluateCekTrace)
import Language.PlutusCore.Evaluation.Result (EvaluationResult(..))
import Language.PlutusCore (Term(..), Constant(..))
import Data.ByteString.Lazy (ByteString)
import GHC.Natural (Natural)
import qualified Language.PlutusCore as PLC
import Unsafe.Coerce (unsafeCoerce)

-- Basic accessors for evaluating results
getInt :: (a, EvaluationResult (Term b c d)) -> Maybe Integer
getInt (_, EvaluationSuccess (Constant _ (BuiltinInt _ _ x))) = Just x
getInt _ = Nothing

getBS :: (a, EvaluationResult (Term b c d)) -> Maybe ByteString
getBS (_, EvaluationSuccess (Constant _ (BuiltinBS _ _ x))) = Just x
getBS _ = Nothing

getSize :: (a, EvaluationResult (Term b c d)) -> Maybe Natural
getSize (_, EvaluationSuccess (Constant _ (BuiltinSize _ s))) = Just s
getSize _ = Nothing

getStr :: (a, EvaluationResult (Term b c d)) -> Maybe String
getStr (_, EvaluationSuccess (Constant _ (BuiltinStr _ x))) = Just x
getStr _ = Nothing

class Unlift a where
  unlift :: DataScript -> Maybe a

instance Unlift Integer where
  unlift (DataScript ds) = getInt $ evaluateCekTrace (scriptToUnderlyingScript ds)

instance Unlift Int where
  unlift (DataScript ds) = fromIntegral <$> (getInt $ evaluateCekTrace (scriptToUnderlyingScript ds))

instance Unlift ByteString where
  unlift (DataScript ds) = getBS $ evaluateCekTrace (scriptToUnderlyingScript ds)

instance Unlift PubKey where
  unlift (DataScript ds) = PubKey <$> unlift (DataScript (extractor `applyScript` ds))
    where
      extractor = $$(Ledger.compileScript [|| \(PubKey x) -> x ||])

type UnderlyingScript = PLC.Program PLC.TyName PLC.Name ()

scriptToUnderlyingScript :: Script -> UnderlyingScript
scriptToUnderlyingScript = unsafeCoerce
