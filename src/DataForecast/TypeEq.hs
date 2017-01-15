{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module     : DataForecast.TypeEq
Descrption : Provides the type equality operator (==?)
-}
module DataForecast.TypeEq where

import DataForecast.Prelude

-- | Type level equality.
type family (==?) (a :: k) (b :: k) :: Bool where
    a ==? a = 'True
    a ==? b = 'False

