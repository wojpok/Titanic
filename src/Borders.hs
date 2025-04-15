{-# LANGUAGE TupleSections #-}

module Borders where

import Types
import Depth
import System.IO.Unsafe
import Data.Either (isLeft)

borderChars :: String
borderChars = "─│╭╮╰╯├┬┼┴┤"

isBorder :: Char -> Bool 
isBorder = flip elem borderChars

--           up   right down  left
type Dir = (Bool, Bool, Bool, Bool)

up :: Dir -> Bool
up (u, _, _, _) = u

right :: Dir -> Bool
right (_, r, _, _) = r

down :: Dir -> Bool
down (_, _, d, _) = d

left :: Dir -> Bool
left (_, _, _, l) = l

dirs :: Char -> Dir
dirs '─' = (False, True, False, True)
dirs '│' = (True, False, True, False)
dirs '╭' = (False, True, True, False)
dirs '╮' = (False, False, True, True)
dirs '╰' = (True, True, False, False)
dirs '╯' = (True, False, False, True)
dirs '├' = (True, True, True, False)
dirs '┬' = (False, True, True, True)
dirs '┴' = (True, True, False, True)
dirs '┤' = (True, False, True, True)
dirs '┼' = (True, True, True, True)
dirs _   = (False, False, False, False)

dirInv :: Dir -> Char
dirInv (False, True, False, True) = '─'
dirInv (True, False, True, False) = '│'
dirInv (False, True, True, False) = '╭'
dirInv (False, False, True, True) = '╮'
dirInv (True, True, False, False) = '╰'
dirInv (True, False, False, True) = '╯'
dirInv (True, True, True, False)  = '├'
dirInv (False, True, True, True)  = '┬'
dirInv (True, True, False, True)  = '┴'
dirInv (True, False, True, True)  = '┤'
dirInv (True, True, True, True)   = '┼'
dirInv _ = undefined


type PChr = Either String Char

mergeDirs :: Dir -> Dir -> Dir
mergeDirs (a, b, c, d) (x, y, z, w) = (a || x, b || y, c || z, d || w)

pointsDir :: PChr -> (Dir -> Bool) -> Bool
pointsDir (Left _)  dir = False
pointsDir (Right x) dir = dir $ dirs x

smush :: PChr -> PChr -> PChr -> PChr -> PChr -> PChr
smush (Right x) u r d l 
  | not (isBorder x) = Right x
  | otherwise = Right 
              $ dirInv 
              $ mergeDirs (dirs x) 
              $ ((pointsDir u down), (pointsDir r left), (pointsDir d up), (pointsDir l right))

type PString = [PChr]

type Zipper a = ([a], [a])

smushBorderLine :: PString -> PString -> PString -> PString -> PString
smushBorderLine _ _ _ [] = []
smushBorderLine u d l (Left xs : z) = Left xs : smushBorderLine u d l z 
smushBorderLine top down left (current : right) =
  let fTail :: [a] -> [a]
      fTail [] = []
      fTail (_:xs) = xs

      mTake :: [PChr] -> PChr
      mTake [] = Left ""
      mTake (Left _:xs) = mTake xs
      mTake (r : _) = r

      mDrop :: [PChr] -> [PChr]
      mDrop (Left _ : xs) = mDrop xs
      mDrop xs = xs

      smushed = smush current (mTake top) (mTake right) (mTake down) (mTake left)
  in smushed : smushBorderLine (mDrop $ fTail top) (mDrop $ fTail down) (smushed:left) right

smushAll :: [PString] -> [PString]
smushAll = reverse . fst . iter . ([], ) where
  sn :: PString -> PString
  sn = dropWhile isLeft

  iter :: Zipper PString -> Zipper PString
  iter ([], x:y:xs) =    iter ([smushBorderLine [] (sn y)      [] x], y:xs)
  iter (y:xs, [x])  =         ((smushBorderLine (sn y) []      [] x) : y : xs, [])
  iter (y:ys, x:x':xs) = iter ((smushBorderLine (sn y) (sn x') [] x) : y : ys, x' : xs)

foldAll :: [PString] -> String
foldAll = concat . map flat where
  flat [] = "\n"
  flat (Left str:xs) = str ++ flat xs
  flat (Right x:xs) = x : flat xs

ppHLine :: Doc
ppHLine = Doc (DCustom () $ \size o _ -> ([LFill "─" size], LFill " " size, size)) $ fixedWidth 0

ppVLine :: Doc
ppVLine = Doc (DCustom () $ \size o _ -> ([], LString " │ ", 3)) $ fixedWidth 3
