module Depth where

-- lista zagnieżdżeń + dł ostatniego elementu
data Shift 
  = Fixed       Int {- >= -} Int 
  | Shift [Int] Int {- >= -} Int

noShift :: Int -> Shift
noShift w = Fixed w w

extendShift :: Int -> Shift -> Shift
extendShift w (Fixed f l) = Fixed (f + w) (l + w)
extendShift w (Shift (s:ss) f l) = Shift (w + s : ss) f l

newShift :: Shift -> Shift
newShift (Fixed f l) = Shift [0] f l
newShift (Shift ss f l) = Shift (0:ss) f l

seqShift :: Shift -> Shift -> Shift
seqShift (Fixed f _) (Fixed f' l') = Fixed (f + f') (f + l')
seqShift (Fixed f _) tl = extendShift f tl
seqShift (Shift ss f _) (Fixed f' l') = Shift ss (f + f') (f + l')
seqShift (Shift ss f _) (Shift (s:ss') f' l') = Shift (ss ++ [f + s] ++ ss') f' l'

stackShift :: Shift -> Shift -> Shift
stackShift (Fixed f _) (Fixed f' l') = Fixed (max f f') l'
-- ???????????
stackSfigt (Fixed f _) (Shift ss f' l') = Shift ss f l'


