
-- ha van egy függvény (a -> Maybe b), akkor az olyan függvény ami hibázhat
-- map, foldr, filter, stb.. nem használható változtatás nélkül

-- ha bárhol nothing-ot ad a kapott fügvény, akkor legyen a végeredmény Nothing
-- egyébként pedig legyen Just <map-elt lista>

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) = case f a of
    Nothing -> Nothing
    Just b  -> case mapMaybe f as of
        Nothing -> Nothing
        Just bs -> Just (b:bs)


-- páros hosszú listák típusa
data List a = Nil | Cons a a (List a)

mapMaybe' :: (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybe' f Nil            = Just Nil
mapMaybe' f (Cons a a' as) = case f a of
    Nothing -> Nothing
    Just b  -> case f a' of
        Nothing -> Nothing
        Just b' -> case mapMaybe' f as of
            Nothing -> Nothing
            Just bs -> Just (Cons b b' bs)


bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f  = Nothing
bind (Just a) f = f a


mapMaybeB :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeB f []     = Just []
mapMaybeB f (a:as) = 
--  bind (f a) (\b -> bind (mapMaybeB f as) (\bs -> Just (b:bs)))
    bind (f a) $ \b ->
    bind (mapMaybe f as) $ \bs ->
    Just (b:bs)


mapMaybeB' :: (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybeB' f Nil            = Just Nil
mapMaybeB' f (Cons a a' as) =
    bind (f a) $ \b ->
    bind (f a') $ \b' ->
    bind (mapMaybeB' f as) $ \bs ->
    Just (Cons b b' bs)

{-
class Functor m => Monad m where
    -- bind függvény: operátor

    -- egymás után végezzük el két mellékhatásos műveletet
    -- második művelet függhet az első végeredményétől 
    -- (a -> m b) kódolja a függőséget
    (>>=) :: m a -> (a -> m b) -> m b
    
    -- (Maybe returnje: Just konstruktor)
    -- (adjunk vissza egy tiszta "a" értéket, mellékhatás nélkül)
    return :: a -> m a

-- minden Monad instance egy custom mellékhatás implementáció
-- két metódus: 1. szekvenciálni tudjunk műveleteket (bind)
--              2. legyen lehetőség mellékhatás nélküli műveletre (return)

instance Monad Maybe where
    return = Just
    (>>=)  = bind
-}



mapMaybeB'' :: (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybeB'' f Nil            = return Nil
mapMaybeB'' f (Cons a a' as) =
    f a >>= \b ->
    f a' >>= \b' ->
    mapMaybeB' f as >>= \bs ->
    return (Cons b b' bs)





