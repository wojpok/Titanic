{-# LANGUAGE TupleSections #-}

module Pretty2 where

import Control.Monad
import Control.Monad.State
import Depth
import Colors
import Types

getWidth :: Doc -> Width
getWidth = snd

ppEmpty :: Doc
ppEmpty = (DEmpty, fixedWidth 0)

ppString :: String -> Doc
ppString str = (DString str, fixedWidth $ length str)

ppShow :: Show a => a -> Doc
ppShow = ppString . show

ppGroup :: Doc -> Doc -> Doc
ppGroup d1 d2@(_, w2) = 
  let d1'@(_, w1) = ppAlignR d1
  in (DEither d1' d2, stackWidth w1 w2)

ppAlignR :: Doc -> Doc
ppAlignR d@(doc, w) = (DAlignR d, resetWidth w)

ppAlignS :: Doc -> Doc 
ppAlignS d@(doc, w) = (DAlignS d, shiftWidth w)

ppSeq :: [Doc] -> Doc
ppSeq xs = (DSeq xs, iter xs) where
    iter :: [Doc] -> Width
    iter [] = fixedWidth 0
    iter (d:ds) = seqWidth (getWidth d) (iter ds)
    
ppStack :: [Doc] -> Doc
ppStack xs = 
  let w = foldr (stackWidth . snd) (fixedWidth 0) xs
  in (DStack xs, w)

ppLayout :: Int -> Doc -> Doc
ppLayout l d@(_, w) = (DLayout l d, w) 

ppColor :: Color -> Doc -> Doc
ppColor c d@(_, w) = (DColor c d, w)

ppBox :: Doc -> Doc
ppBox d@(_, w) = (DBox d, extWidth 1 $ backExtWidth 1 w)

(+++) :: Doc -> Doc -> Doc
(+++) l r = ppSeq [l, r]

pushCol :: Color -> State [Color] ()
pushCol c = do xs <- get
               put (c:xs)

topCol :: [Color] -> Color
topCol (c:_) = c
topCol [] = CWhite

popCol :: State [Color] String
popCol = do xs <- get
            case xs of
              [] -> return ()
              (c:cs) -> put cs
            (ansiColorFg . topCol) <$> get

trColorLine :: [Line] -> [Line]
trColorLine xs = map (\x -> fst $ runState (iter x) []) xs where
  iter :: Line -> State [Color] Line
  iter (LConcat s1 s2) = LConcat <$> iter s1 <*> iter s2
  iter (LColor c)      = pushCol c >> pure (LString (ansiColorFg c))
  iter LColorPop       = LString <$> popCol 
  iter l               = pure l

genLine :: Line -> String
genLine line = iter line [] where
  iter :: Line -> String -> String
  iter (LString str) acc = str ++ acc
  iter (LChar ch) acc = ch:acc
  iter (LConcat s1 s2) acc = iter s1 (iter s2 acc)
  iter (LAlignLeft str) acc = iter str acc
  iter (LFill ch n) acc = iter' n ch acc where
    iter' n ch acc
        | n <= 0    = acc
        | otherwise = iter' (n - 1) ch (ch ++ acc)
  iter LEmpty acc = acc

reduceLines :: [Line] -> Line
reduceLines [] = LEmpty
reduceLines (l:ls) = LConcat l (LConcat (LChar '\n') $ reduceLines ls) 

linesFill :: Int -> Int -> [Line] -> [Line] -> [Line]
linesFill sl sr (l:ls) (r:rs) = LConcat l r : linesFill sl sr ls rs
linesFill sl sr [] (r:rs) = LConcat (LFill " " sl) r : linesFill sl sr [] rs
linesFill sl sr (l:ls) [] = LConcat l (LFill " " sr) : linesFill sl sr ls []
linesFill _ _ [] [] = []

alignDoc :: Int -> CtxBox -> CtxBox 
alignDoc target c@(xs, current) 
  | target == current = c
  | otherwise = (, target) $ do
    x <- xs
    return (LConcat x (LFill " " (target - current)))

{-
- Każdy element dostaje przydział szerokości
  -> Może zająć conajwyżej tyle miesjca, inaczej wejdzie w konflikt z S/R
- Przepychamy odległość od ostatniego Shifta
  -> Reset oblicza z tego odległość do wstrzyknięcia
- Reset oblicza głębokości 
- Seq ma sporo roboty ---- musi podzielić miejsce, między elementy, które nie posiadają shifta

- na razie bez przydziału miejsca
-}

sumFst :: [Int] -> [Int]
sumFst (x:y:tl) = (x + y):tl
sumFst [_] = []

type ShiftState = State [Int]

pop :: ShiftState Int
pop = do xs <- get
         put $ sumFst xs
         return (head xs)

popN :: Int -> ShiftState ()
popN n = forM_ [1..n] $ const pop

shiftBt :: ShiftState a -> ShiftState a
shiftBt m = do st <- get
               res <- m
               put st
               return res

toLines :: Int -> Int -> Doc -> ShiftState CtxBox
toLines size offset (d, w) = do
  let totWidth = width w
  case d of
    DEmpty ->      return ([LEmpty], 0)
    DString str -> return ([LString str], totWidth)
    DAlignS doc -> do
      shift <- pop
      let missingShift = shift - offset
      (tl, ts) <- toLines (size - missingShift) (offset + missingShift) doc
      let ls = linesFill missingShift ts [] tl
      return (ls, (missingShift + ts)) 
    DAlignR doc -> do
      let shifts = shiftList $ getWidth doc
      shiftBt $ do
        put shifts
        toLines size 0 doc
    DSeq ds -> do
      iter size offset ds where
        iter :: Int -> Int -> [Doc] -> ShiftState CtxBox
        iter size off [] = return ([], 0)
        iter size off (d@(_, w):ds) = do
          (os, o) <- toLines size off d 
          (ts, size') <- iter (size - o) (off + o) ds
          let lines = linesFill o size' os ts
          return (lines, (o + size')) 
    DStack ds -> do
      ds' <- mapM (shiftBt . toLines size offset) ds
      let maxSize = foldr (\(_, s) s' -> max s s') 0 ds'
      let aligned = map (\box -> alignDoc maxSize box) ds'
      return (concat (map fst aligned), maxSize)
    DColor col doc -> do
      (lines, w) <- toLines size offset doc
      return (map (\l -> LConcat (LColor col) (LConcat l LColorPop)) lines, w)
    DBox doc -> do
      (lines, w) <- toLines (size - 2) (offset + 1) doc
      let mapped = map (\l -> LConcat (LString "│") (LConcat l (LString "│"))) lines
          topLine    = (LConcat (LString "╭") (LConcat (LFill "─" w) (LString "╮")))
          bottomLine = (LConcat (LString "╰") (LConcat (LFill "─" w) (LString "╯")))
      return ([topLine]  ++ mapped ++ [bottomLine], w + 2)
    DEither doc1 doc2 -> undefined
    --DLayout size' doc -> toLines size' off doc
    --DCustom d cont -> cont size off d
-- toLines = undefined

showDoc :: Doc -> String
showDoc d = do
  let (ls, _) = fst $ runState (toLines 0 0 d) []
  genLine $ reduceLines $ trColorLine ls

sstest = ppAlignR $ ppAlignS $ ppString "def"                
                
stest = ppSeq [ppString "abc", ppAlignS $ ppString "def"]

test = ppAlignR $ ppStack 
        [ ppString "abc " +++ ppAlignS (ppString "= 2137" +++ ppAlignS (ppString ": A")) 
        , ppString "defghi " +++ ppAlignS (ppStack [ppString "= 10" +++ ppAlignS (ppString ": B"), ppString "- 20"])  
        ]
                
testVector = [ ("asdbnashbd", "asdjas")
             , ("asd", "asdasdasdasdsad")
             , ("asd", "asd")
             , ("", "asdasdasdasdddddddddddddddddddddddddddd")
             , ("sssssssssssssssssssssssssssss", "asdddddddddddddddddd")
             ]

ppVec :: [] (String, String) -> Doc
ppVec xs = ppString "let " +++ (ppAlignR $ ppStack $ map (\(l, r) -> ppSeq [ppString l, ppAlignS $ ppString " := ", ppString r]) xs)


doubleShift1 = 
  (ppSeq [ ppString "a"
          , ppAlignS (ppString "bbb")
          , ppString "===="
          , ppAlignS (ppString "c")])

doubleShift2 =
  (ppSeq [ ppString "aaa"
          , ppString "==============="
          , ppAlignS (ppString "b")
          , ppAlignS (ppString "c")])

resetStack = ppAlignR (ppStack [doubleShift1, doubleShift2])

