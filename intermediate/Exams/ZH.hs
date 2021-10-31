{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Bool
import Data.Maybe

-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------
--                              Feladatok
--------------------------------------------------------------------------------

data Tree a = Leaf1 a | Leaf2 a a | Node (Tree a) (Maybe (Tree a))
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  Node
    (Leaf2 2 1)
    (Just (Node
      (Leaf1 10)
      (Just (Node
         (Leaf2 5 6)
         Nothing))))

-- 1
-- instance Functor Tree where
instance Functor Tree where
  fmap f (Leaf1 a) = (Leaf1 (f a))
  fmap f (Leaf2 a b) = (Leaf2 (f a) (f b))
  fmap f (Node left Nothing) = (Node (fmap f left) Nothing)
  fmap f (Node left (Just right)) = (Node (fmap f left) (Just (fmap f right)))

-- 2
-- instance Foldable Tree where
instance Foldable Tree where
  foldr f b (Leaf1 a) = f a b
  foldr f b (Leaf2 a1 a2) = f a1 (f a2 b)
  foldr f b (Node left right) = case right of
    Nothing -> foldr f b left
    (Just r) -> foldr f (foldr f b r) left



-- 1
leftmost :: Tree a -> a
leftmost (Leaf1 a) = a
leftmost (Leaf2 a1 _) = a1
leftmost (Node left _) = leftmost left

-- 2
findInLeaf2 :: (a -> Bool) -> Tree a -> Maybe a
findInLeaf2 _ (Leaf1 _) = Nothing
findInLeaf2 pred (Leaf2 a _) = bool Nothing (Just a) (pred a)
findInLeaf2 pred (Node left right) = case right of
  (Just r) ->  case (findInLeaf2 pred left) of
                Nothing -> findInLeaf2 pred r
                (Just res) -> Just res
  Nothing -> (findInLeaf2 pred left)


-- 1
-- instance Traversable Tree where
instance Traversable Tree where
  traverse f (Leaf1 a)   = Leaf1 <$> f a
  traverse f (Leaf2 a1 a2) = Leaf2 <$> f a1 <*> f a2
  traverse f (Node left right) = case right of
    Nothing -> traverse f left
    Just r  -> Node <$> traverse f left <*> (Just <$> traverse f r)

helper :: Tree a -> Bool
helper (Leaf1 _) = True
helper _ = False

-- 2
countLeaf1s :: Tree a -> Tree (a, Int)
countLeaf1s t = evalState (go t) 0 where
  go a = do
    n <- get
    case a of
      (Leaf1 b) -> put (n+1) >> pure (Leaf1 (b, n))
      (Leaf2 b1 b2) -> pure (Leaf2 (b1, n) (b2, n))
      (Node left right) -> case right of
        Nothing -> do
          left <- go left
          pure (Node left Nothing)
        Just r  -> do
          left <- go left
          r <- go r
          pure (Node left (Just r))

-- 3
replaceLeaf2s :: [a] -> Tree a -> Tree a
replaceLeaf2s l t = evalState (go t) l where
  go a = do
    l <- get
    case a of
      (Leaf1 b) -> pure (Leaf1 b)
      (Leaf2 b1 b2) -> case l of 
        [] -> pure (Leaf2 b1 b2)
        l1:(l2:l) -> put l >> pure (Leaf2 l1 l2)
        l1:l -> put l >> pure (Leaf2 l1 b2)
      (Node left right) -> case right of 
        Nothing -> do
          left <- go left
          pure (Node left Nothing)
        Just r  -> do
          left <- go left
          r <- go r
          pure (Node left (Just r))

-- összesen: 12

--------------------------------------------------------------------------------
--                  While nyelv parser + interpreter
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

anyChar :: Parser Char
anyChar = satisfy (const True)

-- While nyelv
------------------------------------------------------------

data Exp
  = Add Exp Exp    -- a + b
  | Mul Exp Exp    -- a * b
  | Var Name       -- x
  | IntLit Int
  | BoolLit Bool   -- true|false
  | Not Exp        -- not e
  | And Exp Exp    -- a && b
  | Or Exp Exp     -- a || b
  | Eq Exp Exp     -- a == b
  | Lt Exp Exp     -- a < b
  | ReadInt
  | ReadBool
  deriving Show

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}       (lokális scope)
  | Push Exp

  deriving Show


-- While parser
--------------------------------------------------------------------------------

{-
Parser a While nyelvhez. A szintaxist az Exp és Statement definíciónál látahtó
fenti kommentek összegzik, továbbá:

  - mindenhol lehet whitespace tokenek között
  - a Statement-eket egy Program-ban válassza el ';'
  - Az operátorok erőssége és assszociativitása a következő:
      infixr 2 ||
      infixr 3 &&
      infix  4 ==
      infix  4 <
      infixl 6 +
      infixl 7 *
  - "not" erősebben köt minden operátornál.
  - A kulcsszavak: not, and, while, do, if, end, true, false.
  - A változónevek legyenek betűk olyan nemüres sorozatai, amelyek *nem* kulcsszavak.
    Pl. "while" nem azonosító, viszont "whilefoo" már az!

Példa szintaktikilag helyes programra:

  x := 10;
  y := x * x + 10;
  while (x == 0) do
    x := x + 1;
    b := true && false || not true
  end;
  z := x
-}

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]
keywords = ["not", "and", "while", "do", "if", "end", "true", "false", "push", "ReadInt", "ReadBool"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pBoolLit :: Parser Bool
pBoolLit = (True  <$ string' "true")
       <|> (False <$ string' "false")

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

pAtom :: Parser Exp
pAtom =   (ReadInt <$ string' "readInt")
      <|> (ReadBool <$ string' "readBool")
      <|> (BoolLit <$> pBoolLit)
      <|> (IntLit <$> pIntLit)
      <|> (Var <$> pIdent)
      <|> (char' '(' *> pExp <* char' ')')

pNot :: Parser Exp
pNot =
      (Not <$> (string' "not" *> pAtom))
  <|> pAtom

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pNot (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pEqOrLt :: Parser Exp
pEqOrLt =
  pAdd >>= \e ->
        (Eq e <$> (string' "==" *> pAdd))
    <|> (Lt e <$> (string' "<"  *> pAdd))
    <|> pure e

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pEqOrLt (string' "&&")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr 

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pStatement :: Parser Statement
pStatement =
        (Assign <$> pIdent <*> (string' ":=" *> pExp))
    <|> (While <$> (string' "while" *> pExp)
               <*> (string' "do" *> pProgram <* string' "end"))
    <|> (If <$> (string' "if"   *> pExp)
            <*> (string' "then" *> pProgram)
            <*> (string' "else" *> pProgram <* string' "end"))
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))
    <|> (Push <$> (string' "push" *> pExp) )

pSrc :: Parser Program
pSrc = ws *> pProgram <* eof



-- Interpreter
------------------------------------------------------------

{-
Interpreter a While nyelvhez.

Kifejezések:
  - A logikai és artimetikai műveletek kiértékelése értelemszerű. Ha nem
    megfelelő típusú értéket kapunk argumentumokra, dobjunk "error"-al hibát.
  - Az == operátor működik, ha mindkét argumentum Bool, vagy ha mindkét argumentum
    Int, az eredmény mindig Bool.

Változó scope és értékadás kezelése:
  - Új scope-nak számít:
    - minden "while" kifejezés teste
    - minden "if" kifejezés két ága
    - minden új Block (a szintaxisban pl "x := 0; {y := x; x := x}"-nél
      a kapcsos zárójeles utasítássorozat új blokkban van).

  - ha egy új változónak értéket adunk, akkor felvesszük a környezet elejére
  - ha egy meglévő változónak értéket adunk, akkor update-eljük a változó értékét
  - amikor az interpreter végez egy scope kiértékeléséval, eldobja az összes,
    scope-ban újonnan felvett változót a környezetből.
-}

type Val  = Either Int Bool
type Env  = [(Name, Val)]
type Eval = State (Env, [String]) 

binOp :: String
      -> Exp
      -> Exp
      -> Either (Int -> Int -> Int) (Bool -> Bool -> Bool)
      -> Eval Val
binOp opName e1 e2 f = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (f, v1, v2) of
    (Left f , Left n1 , Left n2 ) -> pure (Left  (f n1 n2))
    (Right f, Right b1, Right b2) -> pure (Right (f b1 b2))
    _                             -> error ("type error in " ++ opName ++ " argument")

evalExp :: Exp -> Eval Val
evalExp e = case e of
  Add e1 e2 -> binOp "+"  e1 e2 (Left (+))
  Mul e1 e2 -> binOp "*"  e1 e2 (Left (*))
  And e1 e2 -> binOp "&&" e1 e2 (Right (&&))
  Or  e1 e2 -> binOp "||" e1 e2 (Right (||))
  Eq  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left  n1, Left  n2) -> pure (Right (n1 == n2))
      (Right b1, Right b2) -> pure (Right (b1 == b2))
      _                    -> error "type error in == arguments"
  Lt  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (Left  n1, Left  n2) -> pure (Right (n1 < n2))
      _                    -> error "type error in < arguments"
  Var x -> do
    env <- get
    case lookup x (fst env) of
      Nothing -> error ("variable not in scope: " ++ x)
      Just v  -> pure v
  IntLit n -> pure (Left n)
  BoolLit b -> pure (Right b)
  Not e -> do
    v <- evalExp e
    case v of
      Right b -> pure (Right (not b))
      _       -> error "type error in \"not\" argument"
  ReadInt -> do
    env <- get
    let resInt = fromJust (runParser (ws *> pIntLit <* eof) (head $ snd env))
    modify (\(env, stream) -> (env, (tail stream)))
    pure (Left $ fst resInt)
  ReadBool -> do
    env <- get
    let resBool = fromJust (runParser (ws *> pBoolLit <* eof) (head $ snd env))
    modify (\(env, stream) -> (env, (tail stream)))
    pure (Right $ fst resBool)

newScope :: Eval a -> Eval a
newScope ma = do
  env <- (get :: State (Env, [String]) (Env, [String]))
  a <- ma
  modify (\(env', stream') -> (drop (length env' - length (fst env)) env', stream'))
  pure a

updateEnv :: Name -> Val -> Env -> Env
updateEnv x v env =
  case go env of
    Nothing   -> (x, v):env
    Just env' -> env'
  where
    go :: Env -> Maybe Env
    go [] = Nothing
    go ((x', v'):env)
      | x == x'   = Just ((x, v):env)
      | otherwise = ((x', v'):) <$> go env

evalSt :: Statement -> Eval ()
evalSt s = case s of
  Assign x e -> do
    v <- evalExp e
    modify (\(env, stream) -> ((updateEnv x v env), stream))
  While e p -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p) >> evalSt (While e p)
                      else pure ()
      _       -> error "type error: expected a Bool condition in \"while\" expression"
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      Right b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      _       -> error "type error: expected a Bool condition in \"if\" expression"
  Block p ->
    newScope (evalProg p)

evalProg :: Program -> Eval ()
evalProg = mapM_ evalSt

removeUnusedVars :: Maybe (Program, String) -> Maybe (Program, String)
removeUnusedVars program = case program of
  Nothing -> Nothing
  Just (p, s) -> Just (p, s)

-- interpreter
--------------------------------------------------------------------------------

runProg :: String -> [String] -> (Env, [String])
runProg str stream = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (p, _) -> execState (evalProg p) ([], stream)

p1 :: String
p1 = "x := 10; y := 20; {z := x + y}; x := x + 100"


pEx0 :: String
pEx0 = "push x"

pEx1 :: String
pEx1 = "x := 5; y := 7; z := y + 1; push x"