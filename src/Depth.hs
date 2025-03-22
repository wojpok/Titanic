module Depth where

data Width
  = Shift Int Int Width
  | Fixed Int
  deriving (Show, Eq, Ord)

fixedWidth :: Int -> Width
fixedWidth = Fixed

shiftWidth :: Width -> Width
shiftWidth = Shift 0 0

resetWidth :: Width -> Width
resetWidth = fixedWidth . width

width :: Width -> Int
width (Fixed f) = f
width (Shift f b tl) = max b (f + width tl)

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
extWidth w (Fixed f) = Fixed (w + f)
extWidth w (Shift f b tl) = Shift (f + w) (b + w) tl

backExtWidth :: Int -> Width -> Width
backExtWidth w (Fixed f) = Fixed (w + f)
backExtWidth w (Shift f b tl) = Shift f (b + w) (backExtWidth w tl)

shiftCount :: Width -> Int
shiftCount (Fixed _) = 0
shiftCount (Shift _ _  tl) = 1 + shiftCount tl

shiftList :: Width -> [Int]
shiftList (Fixed _) = []
shiftList (Shift f _ tl) = f : shiftList tl
