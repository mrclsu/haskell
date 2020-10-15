{-# LANGUAGE InstanceSigs, KindSignatures #-}
module Practice5 where 
  import Data.List

  isJust :: Maybe a -> Bool
  isJust (Just _) = True
  isJust _ = False

  fromJust :: Maybe a -> a
  fromJust (Just a) = a
  fromJust Nothing = error "can't extract value from Nothing"

  catMaybes :: [Maybe a] -> [a]
  catMaybes [] = []
  catMaybes ((Nothing) : as) = catMaybes as
  catMaybes ((Just a) : as) = a : catMaybes as

  mapMaybe :: (a -> Maybe b) -> [a] -> [b]
  mapMaybe = (catMaybes .) . map

  safeHead :: [a] -> Maybe a
  safeHead [] = Nothing
  safeHead (a:_) = Just a

  fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
  fmapMaybe = fmap

  bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
  bindMaybe = (>>=)

  {- tests
  bindMaybe Nothing id                  == Nothing
  bindMaybe (Just 2) (\x -> Just (2*x)) == Just 4
  bindMaybe (Just 2) (\x -> Nothing)    == Nothing

  returnMaybe 5 == Just 5
  -}

  -- fira code



-- Test 5
data NatF a = ZeroF | SucF a
  deriving (Eq, Ord, Show)

instance Functor NatF where 
  fmap :: (a -> b) -> NatF a -> NatF b
  fmap _ ZeroF = ZeroF
  fmap f (SucF a) = (SucF (f a))


data NonEmpty a = NonEmpty a [a]
  deriving (Eq, Ord, Show)

instance Functor NonEmpty where 
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (NonEmpty a as) = NonEmpty (f a) (map f as)