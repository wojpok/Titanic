{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module FLFconfig where

import Data.Bits
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import Data.Maybe (fromMaybe, isNothing, isJust)

type Set = S.Set
type Map = M.Map

flag :: Int -> Int
flag = shiftL 1

-- Horizontal rules
smushHorizontal1 :: Int
smushHorizontal1    = flag 0
smushHorizontal2 :: Int
smushHorizontal2    = flag 1
smushHorizontal3 :: Int
smushHorizontal3    = flag 2
smushHorizontal4 :: Int
smushHorizontal4    = flag 3
smushHorizontal5 :: Int
smushHorizontal5    = flag 4
smushHorizontal6 :: Int
smushHorizontal6    = flag 5
fittingHorizontal :: Int
fittingHorizontal   = flag 6
smushingHorizontal :: Int
smushingHorizontal  = flag 7

-- Vertical rules
smushVertical1 :: Int
smushVertical1      = flag 8
smushVertical2 :: Int
smushVertical2      = flag 9
smushVertical3 :: Int
smushVertical3      = flag 10
smushVertical4 :: Int
smushVertical4      = flag 11
smushVertical5 :: Int
smushVertical5      = flag 12
fittingVertical :: Int
fittingVertical     = flag 13
smushingVertical :: Int
smushingVertical    = flag 14


type Smush = Char -> Char -> Maybe Char

type SmushRule = Char -> [((Char, Char), Char)]

buildHorizontalSmushFunction :: Int -> Char -> Smush
buildHorizontalSmushFunction mode hardblank = do
    let xs = confs <*> [mode] <*> [hardblank]

    let x = reduce xs $ dflSmush hardblank

    \c1 c2 -> M.lookup (c1, c2) $ M.fromList x

    where
        decode :: Int -> SmushRule -> Int -> Char -> [((Char, Char), Char)] -> [((Char, Char), Char)]
        decode flag rule mode hardblank tl
            | flag .&. mode == flag = rule hardblank ++ tl
            | otherwise = tl

        confs = [ decode smushHorizontal1 smushRule1
                , decode smushHorizontal2 smushRule2
                , decode smushHorizontal3 smushRule3
                , decode smushHorizontal4 smushRule4
                , decode smushHorizontal5 smushRule5
                , decode smushHorizontal6 smushRule6
                ]

        reduce = foldl (.) id

        -- reduce [] xs = xs
        -- reduce (f:fs) xs = reduce fs (f xs)

dflSmush :: SmushRule
dflSmush _ = do
    x <- C.chr <$> [32..126]
    [((x, ' '), x), ((' ', x), x)]

smushRule1 :: SmushRule
smushRule1 hardblank =
    let code = C.ord hardblank in
    (\x -> ((x, x), x)) . C.chr <$> ([32 .. code - 1] ++ [code + 1 .. 126])

smushRule2 :: SmushRule
smushRule2 _ = do
    chr <- [ '|'
            , '/', '\\'
            , '[', ']'
            , '{', '}'
            , '(', ')'
            , '<', '>'
            ]
    [ (('_', chr), chr), ((chr, '_'), chr)]

smushRule3 :: SmushRule
smushRule3 _ = do
    let priority = concat
            $ zipWith (\n -> map (n,)) [1 :: Int ..]
              [ "|"
              , "/\\"
              , "[]"
              , "{}"
              , "()"
              , "<>"
              ]

    (p1, c1) <- priority
    (p2, c2) <- priority

    if p1 > p2 then
        return ((c1, c2), c1)
    else if p1 < p2 then
        return ((c1, c2), c2)
    else
        []

smushRule4 :: SmushRule
smushRule4 _ = [ ((']', '['), '|')
               , ((')', '('), '|')
               , (('}', '{'), '|')
               ]

smushRule5 :: SmushRule
smushRule5 _ = [ (('/', '\\'), '|')
               , (('\\', '/'), 'Y')
               , (('>', '<'), 'X')
               ]

smushRule6 :: SmushRule
smushRule6 hardblank = [((hardblank, hardblank), hardblank)]



letter1 :: [String]
letter1 = 
    [ " _   __"
    , "(_) / /"
    , "   / / "
    , "  / /  "
    , " / / _ "
    , "/_/ (_)"
    , "       "
    ]

letter2 :: [String]
letter2 =
    [ "   _  _   "
    , " _| || |_ "
    , "|_  __  _|"
    , " _| || |_ "
    , "|_  __  _|"
    , "  |_||_|  "
    , "          "
    ]



type Lines = [String]

type LineZipper = (Lines, Lines)

initLineZipper :: Lines -> LineZipper
initLineZipper xss = (map reverse xss, map (const []) xss)

goLeft :: LineZipper -> LineZipper
goLeft (yss, xss) = do
    let step = map head yss
    let tl = map tail yss
    let next = zipWith (:) step xss
    (tl, next)

testSmush :: Smush
testSmush = buildHorizontalSmushFunction (flag 15 - 1) '$'

checkKern :: Smush -> LineZipper -> Lines -> [[Maybe Char]]
checkKern smush (_, xss) = zipWith (zipWith smush) xss


z = initLineZipper letter1
k = checkKern testSmush z letter2

makeKern :: Smush -> LineZipper -> Lines -> Maybe Lines
makeKern smush (zss, xss) yss = do
    let sol = checkKern smush ([], xss) yss

    if any (any isNothing) sol then
        Nothing
    else
        Just $ zipWith (\x y -> reverse x ++ y) zss $ zipWith mergeLine sol yss
    
        where
            mergeLine :: [Maybe Char] -> String -> String
            mergeLine (Just c:chrs) (_:tl) = c : mergeLine chrs tl
            mergeLine [] str = str
            mergeLine _ _ = undefined

t1 = makeKern testSmush z letter2
t2 = makeKern testSmush (goLeft z) letter2
t3 = makeKern testSmush (goLeft $ goLeft z) letter2
t4 = makeKern testSmush (goLeft $ goLeft $ goLeft z) letter2

t5 = makeKern testSmush (goLeft $ goLeft $ goLeft $ goLeft z) letter2
t6 = makeKern testSmush (goLeft $ goLeft $ goLeft $ goLeft $ goLeft z) letter2

showMaybeLines :: Maybe Lines -> IO ()
showMaybeLines Nothing = return ()
showMaybeLines (Just ls) = mapM_ putStrLn ls




