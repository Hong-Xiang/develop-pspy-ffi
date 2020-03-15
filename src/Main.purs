module Main where

import Prelude
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)

import Effect (Effect)
import Effect.Console (log)

gen :: Int -> Maybe (Tuple Int Int)
gen 0 = Nothing
gen n = Just (Tuple n (n - 1))

values :: Array Int
values = unfoldr gen 10

main :: Effect Unit
main = do
  log $ show values
