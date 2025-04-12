module Depth where

import System.IO.Unsafe

{-
Algorytm przydziału miejsca
- pudełka definiują swoją rozszerzalność (skala u-intowa)
- zapamiętujemy maksymaloną sumaryczną rozszerzalność między shiftami
- reset będzie mógł prydzielić miejsce w dwóch strategiach
  -> minimalnej (ściśniętej - tak jak teraz) - ignoruje rozszerzalność i bierze minimum
  -> rozwiniętej - alokuje pozycjonowanie shiftów proporcjonalnie do deklarowanej rozszerzalności
     między shiftami
-}


type MinWidth = Int
type MaxWidth = Int
type WScaling = Int

data Scale = Scale MinWidth MaxWidth WScaling

instance Show Scale where
  show (Scale x y z) = "(" ++ show x ++ "-" ++ show y ++ " " ++ show z ++ ")"

instance Num Scale where
  (Scale x y z) + (Scale a b c) = Scale (x + a) (y + b) (z + c)
  (-) = undefined
  (*) = undefined
  signum _ = 0
  fromInteger = undefined
  abs = id

instance Eq Scale where
  (Scale x _ _) == (Scale x' _ _) = x == x'

instance Ord Scale where
  (Scale x _ _) <= (Scale x' _ _) = x <= x'

data Width
  = Shift Scale Scale Width
  | Fixed Scale
  deriving (Show, Eq, Ord)

data Spread
  = Sparse
  | Float

fixedScale :: Int -> Scale
fixedScale x = Scale x x 0

flexScale :: Int -> Scale
flexScale y = Scale 0 y 1

fixedWidth :: Int -> Width
fixedWidth = Fixed . fixedScale 

addFlexWidth :: Int -> Width -> Width
addFlexWidth f (Fixed s) = Fixed (s + flexScale f)
addFlexWidth f (Shift s1 s2 tl) = Shift (flexScale f + s1) (flexScale f + s2) tl

shiftWidth :: Width -> Width
shiftWidth = Shift (Scale 0 0 0) (Scale 0 0 0)

resetWidth :: Width -> Width
resetWidth = Fixed . width

width :: Width -> Scale
width (Fixed f) = f
width (Shift f b tl) = max b (f + width tl)

minWidth :: Scale -> Int
minWidth (Scale m _ _) = m

maxWidth :: Scale -> Int
maxWidth (Scale _ m _) = m

scaleWidth :: Scale -> Int
scaleWidth (Scale _ _ s) = s

seqWidth :: Width -> Width -> Width
seqWidth (Fixed f) (Fixed f') = Fixed (f + f')
seqWidth (Fixed f) (Shift f' b' tl) = Shift (f + f') (f + b') tl
seqWidth (Shift f b tl) w = Shift f b (seqWidth tl w) 

stackWidth :: Width -> Width -> Width
stackWidth (Fixed f) (Fixed f') = Fixed (max f f')
stackWidth (Fixed f) (Shift f' b' tl') = Shift f' (max f b') tl'
stackWidth (Shift f b tl) (Shift f' b' tl') = Shift (max f f') (max b b') (stackWidth tl tl')
stackWidth (Shift f b tl) (Fixed f') = Shift f (max b f') tl

extWidth :: Int -> Width -> Width
extWidth w (Fixed f) = Fixed (fixedScale w + f)
extWidth w (Shift f b tl) = Shift (f + fixedScale w) (b + fixedScale w) tl

backExtWidth :: Int -> Width -> Width
backExtWidth w (Fixed f) = Fixed (fixedScale w + f)
backExtWidth w (Shift f b tl) = Shift f (b + fixedScale w) (backExtWidth w tl)

shiftCount :: Width -> Int
shiftCount (Fixed _) = 0
shiftCount (Shift _ _  tl) = 1 + shiftCount tl

shiftList :: Width -> [Scale]
shiftList (Fixed f) = [f]
shiftList (Shift f _ tl) = f : shiftList tl

shiftAllocation :: Spread -> Int -> [Scale] -> [Int]
shiftAllocation Sparse _ scales = map minWidth scales
shiftAllocation Float max scales =
  let mins = map minWidth scales
      scs  = map scaleWidth scales
      tot  = sum mins
    
      toSpend = max - tot
      avgSpend = toSpend `div` sum scs
      leftover = toSpend - sum scs * avgSpend
  
      allocate :: Int -> [(Int, Int)] -> [Int]
      allocate leftover [(x, y)] = [x + leftover]
      allocate leftover ((x, y):xs)
        | leftover <= 0 = x : map fst xs
        | otherwise = let alloc = min leftover y
                      in (x + alloc) : allocate (leftover - alloc) xs

  in if sum scs == 0 then
    shiftAllocation Sparse max scales
  else 
    let res = allocate leftover $ zipWith (\x y -> (x + avgSpend * y, y)) mins scs
    in res
 