module Main where

import Prelude
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (unfoldr)

import Effect (Effect)
import Effect.Console (log, logShow)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Unsafe.Coerce (unsafeCoerce)

gen :: Int -> Maybe (Tuple Int Int)
gen 0 = Nothing
gen n = Just (Tuple n (n - 1))

values :: Array Int
values = unfoldr gen 10

sumOfSquares :: Int
sumOfSquares = ST.run do
  total <- STRef.new 0
  let loop 0 = STRef.read total
      loop n = do
        _ <- STRef.modify (_ + (n * n)) total
        loop (n - 1)
  loop 100

newtype Foo = Foo String

newtype Bar = Bar String

-- | The two newtypes `Foo` and `Bar` have the same runtime representation,
-- | so it is safe to coerce one into the other directly.
coerceFoo :: Foo -> Bar
coerceFoo = unsafeCoerce

-- | It is also safe to coerce entire collections, without having to map over
-- | individual elements.
coerceFoos :: forall f. Functor f => f Foo -> f Bar
coerceFoos = unsafeCoerce


main :: Effect Unit
main = do
-- --   log $ show values
--     ref <- ST.new 1
--     cur <- ST.read ref
--     log $ show $ cur
--     ST.write 2 ref
--     cur2 <- ST.read ref
--     log $ show $ cur2
    -- logShow sumOfSquares
    case coerceFoos [Foo "Hello", Foo " ", Foo "World"] of
        [Bar x, Bar y, Bar z] -> log (x <> y <> z)
        _ -> log "impossible"

