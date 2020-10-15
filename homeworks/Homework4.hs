module LastHomework where 
  import Data.List
  import Data.Bool

  isJust :: Maybe a -> Bool
  isJust Nothing = False
  isJust _ = True

  fromJust :: Maybe a -> a
  fromJust (Just a) = a
  fromJust _ = error "bad :)"

  catMaybes :: [Maybe a] -> [a]
  catMaybes [] = []
  catMaybes ((Nothing) : as) = catMaybes as
  catMaybes ((Just a) : as) = a : catMaybes as

  mapMaybe :: (a -> Maybe b) -> [a] -> [b]
  mapMaybe = (catMaybes .) . map

  safeHead :: [a] -> Maybe a
  safeHead [] = Nothing
  safeHead (a:_) = Just a


  type Username = String
  type Password = String

  data Privilege = Simple | Admin
    deriving (Eq, Show)

  data Cookie = LoggedOut | LoggedIn Username Privilege
    deriving (Eq, Show)

  data Entry = Entry Password Privilege [Username]
    deriving (Eq, Show)

  type Database = [(Username, Entry)]


  richard, charlie, carol, david, kate :: (Username, Entry)
  richard = ("Richard", Entry "password1" Admin  ["Kate"])
  charlie = ("Charlie", Entry "password2" Simple ["Carol"])
  carol   = ("Carol",   Entry "password3" Simple ["David", "Charlie"])
  david   = ("David",   Entry "password4" Simple ["Carol"])
  kate    = ("Kate",    Entry "password5" Simple ["Richard"])

  testDB :: Database
  testDB = [ richard, charlie, carol, david, kate ]

  testDBWithoutCarol :: Database
  testDBWithoutCarol =
    [ ("Richard", Entry "password1" Admin  ["Kate"])
    , ("Charlie", Entry "password2" Simple [])
    , ("David",   Entry "password4" Simple [])
    , ("Kate",    Entry "password5" Simple ["Richard"])
    ]


  password :: Entry -> Password
  password (Entry password _ _) = password

  privilege :: Entry -> Privilege
  privilege (Entry _ privilege _) = privilege

  friends :: Entry -> [Username]
  friends (Entry _ _ friends) = friends

  mkCookie :: Username -> Password -> Entry -> Cookie
  mkCookie uname providedPasswd (Entry passwd priv _) = bool LoggedOut (LoggedIn uname priv) $ providedPasswd == passwd

  login :: Username -> Password -> Database -> Cookie
  login uname passwd db = mkCookie uname passwd (maybe (Entry "" Simple []) (id) $ lookup uname db )

  updateEntry :: Username -> (Username, Entry) -> Maybe (Username, Entry)
  updateEntry uname (uname2, (Entry passwd priv friends)) = bool (Just (uname2, (Entry passwd priv $ filter (/=uname) friends))) Nothing $ uname == uname2

  deleteUser :: Cookie -> Username -> Database -> Database
  deleteUser (LoggedIn _ Admin) uname db = mapMaybe (updateEntry uname) db
  deleteUser _ _ db = db

  -- part 2:
  getFriends :: Username -> Database -> [Username]
  getFriends uname db = maybe [] (\(Entry _ _ friends) -> friends) (lookup uname db) 

  getFriendsRefl :: Username -> Database -> [Username]
  getFriendsRefl uname db = bool [] (uname:(getFriends uname db)) $ (lookup uname db) /= Nothing

  fixPoint :: Eq a => (a -> a) -> a -> a
  fixPoint f a = bool (fixPoint f (f a)) (f a) $ (f a) == (f (f a))

  sortUnique :: (Eq a, Ord a) => [a] -> [a]
  sortUnique =  unique . sort  where
    unique [] = []
    unique (x:xs) = x : unique (dropWhile (==x) (x:xs))

  getSocialNetwork :: Username -> Database -> [Username]
  getSocialNetwork uname db = fixPoint (sortUnique . concatMap ((flip getFriends) db)) $ getFriendsRefl uname db