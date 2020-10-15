import Data.Char
import Data.List
import qualified Data.Set as Set

safeDiv :: Double -> Double -> Maybe Double
safeDiv a 0 = Nothing
safeDiv a b = Just $ a / b


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


getVal :: Maybe a -> a
getVal Nothing = error "This is Nothing"
getVal (Just a) = a


lookUpDef :: Eq k => k -> [(k, v)] -> v -> v
lookUpDef _ [] v = v
lookUpDef k ((x, y):xs) v
    | k == x = y
    | otherwise = lookUpDef k xs v


lookUpDef' :: Eq k => k -> [(k,v)] -> Maybe v
lookUpDef' _ [] = Nothing
lookUpDef' k ((x, y):xs)
    | k == x = Just y
    | otherwise = lookUpDef' k xs





morseTab :: [(Char, String)]
morseTab =
    [('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),('E',".")
    ,('F',"..-."),('G',"--."),('H',"...."),('I',".."),('J',".---")
    ,('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),('O',"---")
    ,('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),('T',"-")
    ,('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-")
    ,('Y',"-.--"),('Z',"--..")
    ]

normalizeText :: String -> String
normalizeText str = filter (\c -> elem c ['A'..'Z']) $ map toUpper str 

charToCode :: [(Char,String)] -> Char -> String
charToCode map c = 
    case lookup c map of
        Just v -> v
        Nothing -> error "Invalid Char"

encodeToWords :: String -> [String]
encodeToWords str = map (charToCode morseTab) $ normalizeText str

encodeString :: String -> String
encodeString = concat . encodeToWords

codeToChar :: [(a,String)] -> String -> a
codeToChar mp m =
    case lookup m (map (\(a,b) -> (b,a)) mp) of
        Just v -> v
        Nothing -> error "Invalid Morse Code"


decodeWords :: [String] -> String
decodeWords = map $ codeToChar morseTab


withShortestCodes :: [(Char, String)] -> [Char]
withShortestCodes codeing = map fst ((filter (\(_,s) -> minLen == length s) codeing))
    where
        minLen = minimum $ map (length . snd) codeing


getPossiblePrefixes :: [(Char,String)] -> String -> [(Char,String)]
getPossiblePrefixes mt st = filter (\(_,c) -> isPrefixOf c st) mt


powersets :: [a] -> [[a]]
powersets [] = [[]]
powersets (x:xs) = xss ++ map (x:) xss
    where xss = powersets xs

powerset :: String -> [String]
powerset str = nub $ filter (\x-> not $ null $ getPossiblePrefixes morseTab x) $ powersets str

decodeString :: String -> [String]
decodeString "" = [""]
decodeString [x] = [[fst (head $ getPossiblePrefixes morseTab [x])]]
decodeString str = [ p : s | p <- prefixes, s <- (decodeString (drop (length (charToCode morseTab p)) str)) ]
    where
        prefixes = map (\(c, _) -> c) $ getPossiblePrefixes morseTab str

