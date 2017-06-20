module Data.Generic.Rep.Enum
  ( class GenericEnum
  , genericEnum'
  , genericSucc'
  , genericPred'
  , Recur(..)
  , genericEnum
  , genericSucc
  , genericPred
  ) where

import Prelude
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Maybe (Maybe(..))

data Recur a = ReadConstructor | Recur a

class GenericEnum a where
  genericEnum' :: Array a
  genericSucc' :: Recur a -> Maybe a
  genericPred' :: Recur a -> Maybe a

instance genericEnumConstructor :: GenericEnum (Constructor name NoArguments) where
  genericEnum' = [ Constructor NoArguments ]

  genericSucc' ReadConstructor = Just $ Constructor NoArguments
  genericSucc' (Recur _)       = Nothing

  genericPred' ReadConstructor = Just $ Constructor NoArguments
  genericPred' (Recur _)       = Nothing

instance genericEnumSum :: (GenericEnum a, GenericEnum b) => GenericEnum (Sum a b) where
  genericEnum' = (Inl <$> genericEnum') <> (Inr <$> genericEnum')

  genericSucc' (Recur (Inl _)) = Inr <$> genericSucc' ReadConstructor
  genericSucc' (Recur (Inr x)) = Inr <$> genericSucc' (Recur x)
  genericSucc' ReadConstructor = Inl <$> genericSucc' ReadConstructor

  genericPred' (Recur (Inl _)) = Nothing
  genericPred' (Recur (Inr x)) = case genericPred' (Recur x) of
                                  Nothing  -> Inl <$> genericPred' ReadConstructor
                                  (Just z) -> Just $ Inr z
  genericPred' ReadConstructor = Nothing

genericSucc :: forall a rep. Generic a rep => GenericEnum rep => a -> Maybe a
genericSucc a = to <$> genericSucc' (Recur $ from a)

genericPred :: forall a rep. Generic a rep => GenericEnum rep => a -> Maybe a
genericPred a = to <$> genericPred' (Recur $ from a)

genericEnum :: forall a rep. Generic a rep => GenericEnum rep => Array a
genericEnum = to <$> genericEnum'
