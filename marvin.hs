import Control.Applicative
import Debug.Trace
import Data.List

main = print $ decode (Config deLayout 8 4 enFrequentCharacters) "ahlbtr krtt afhb ldjnvrtlkrvn kvaadumk ie cfstlaqtf"


deLayout :: Layout
deLayout = 
    [ "qwertzuiop"
    , "asdfghjkl"
    , "yxcvbnm"
    ]
    
enFrequentCharacters :: String
enFrequentCharacters = "etaoinsrhldcumfpgwybvkxjqz"


type Layout = [String]

data Config = Config { keyboardLayout :: Layout
                     , splitLength :: Int
                     , rowDistance :: Int
                     , frequentChars :: String
                     }

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair x y) = 
        Pair (f x) (f y)

instance Applicative Pair where
    (Pair fx fy) <*> (Pair x y) =
        Pair (fx x) (fy y)
    pure x =
        Pair x x

instance Show a => Show (Pair a) where
    show (Pair a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"

type Position = Pair Int

addP :: Position -> Position -> Position
addP a b = (+) <$> a <*> b

subP :: Position -> Position -> Position
subP a b = (-) <$> a <*> b



dist :: Config -> Position -> Position -> Int
dist conf p1 p2 =
    case subP p1 p2 of
        Pair dx dy ->
            abs dx + rowDistance conf * abs dy


isLowercaseLetter :: Char -> Bool
isLowercaseLetter c = 
    c >= 'a' && c <= 'z'



indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f lst =
    map (uncurry f) $ zip [0..] lst


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeListIndex :: [a] -> Int -> Maybe a
safeListIndex [] _ = Nothing
safeListIndex (x:xs) 0 = Just x
safeListIndex (x:xs) n = safeListIndex xs (n-1)

getPosition :: Config -> Char -> Maybe Position
getPosition conf c =
    let
        layout = keyboardLayout conf
        indexedLayout = concat $ indexedMap (\y lst -> indexedMap (\x a -> (Pair x y, a)) lst) layout
    in
        fmap fst $ find (\(_,char) -> c == char) indexedLayout
        
decodeNextChar :: Config -> (Maybe Position, Int, Maybe Char) -> Char -> (Maybe Position, Int, Maybe Char)
decodeNextChar conf (currentPos, partialCharSum, _) c =
    case getPosition conf c of
        Nothing ->
            (currentPos, partialCharSum, Just c)
        Just pos ->
            case currentPos of
                Nothing -> 
                    (Just pos, partialCharSum, Nothing)
                Just cPos ->
                    let
                        currentDist = dist conf cPos pos
                        newSum = partialCharSum + currentDist
                        partialSumDone = ((newSum `mod` splitLength conf) /= 0) || (currentDist == 0)
                    in
                        if partialSumDone then
                            (Just pos, 0, safeListIndex (frequentChars conf) newSum)
                        else
                            (Just pos, newSum, Nothing)


-- returns Nothing if ciphertext ends with an unterminated partial character
decode :: Config -> String -> Maybe String
decode conf ciphertext =
    let
        scanned = scanl (decodeNextChar conf) (Nothing, 0, Nothing) ciphertext
    in 
        case safeLast scanned of
            Just (_, 0, _) -> 
                Just [ c | (_, _, Just c) <- scanned]
            _ -> 
                Nothing

encode :: Config -> String -> String
encode conf plaintext =
    plaintext
