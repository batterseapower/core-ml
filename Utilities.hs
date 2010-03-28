module Utilities (
    module Utilities,
    
    module Control.Arrow,
    
    module Debug.Trace,
    
    module Text.PrettyPrint.HughesPJClass
  ) where

import Control.Arrow (first, second)

import Debug.Trace

import Text.PrettyPrint.HughesPJClass


appPrec, opPrec, noPrec :: Rational
appPrec = 2 -- Argument of a function application
opPrec  = 1 -- Argument of an infix operator
noPrec  = 0 -- Others

haskellLevel :: PrettyLevel
haskellLevel = PrettyLevel 1


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

takeFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
takeFirst _ [] = (Nothing, [])
takeFirst p (x:xs)
  | p x       = (Just x, xs)
  | otherwise = second (x:) $ takeFirst p xs

expectJust :: String -> Maybe a -> a
expectJust _ (Just x) = x
expectJust s Nothing  = error $ "expectJust: " ++ s

notNull :: [a] -> Bool
notNull = not . null
