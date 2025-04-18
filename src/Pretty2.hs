{-# LANGUAGE TupleSections, Strict, LambdaCase #-}

module Pretty2 where

import Control.Monad
import Control.Monad.State
import Depth
import Colors
import Types
import System.IO.Unsafe
import GHC.Utils.Misc
import Borders

{-
Algebra kombinatorów

flex n <+> flex m = flex (n + m)
ppSeq (x :: xs) = x <+> ppSeq xs
ppSeq [x] = x
(x <+> y) <+> z = x <+> (y <+> z)
&ppSeq (x :: xs) = &x <+> ppSeq xs = ppSeq (&x :: xs)

Do dodania:
  - znaki trailujące
  - boxy
-}

getWidth :: Doc -> Width
getWidth (Doc _ snd) = snd

ppEmpty :: Doc
ppEmpty = Doc DEmpty $ fixedWidth 0

ppString :: String -> Doc
ppString str = Doc (DString str) $ fixedWidth $ length str

ppShow :: Show a => a -> Doc
ppShow = ppString . show

ppGroup :: Doc -> Doc -> Doc
ppGroup d1 d2@(Doc _ w2) = 
  let d1'@(Doc _ w1) = ppAlignR d1
  in Doc (DEither d1' d2) $ stackWidth w1 w2

ppAlignR :: Doc -> Doc
ppAlignR d@(Doc doc w) = Doc (DAlignR d) $ resetWidth w

ppAlignS :: Doc -> Doc 
ppAlignS d@(Doc doc w) = Doc (DAlignS d) $ shiftWidth w

ppSeq :: [Doc] -> Doc
ppSeq xs = Doc (DSeq xs) $ iter xs where
    iter :: [Doc] -> Width
    iter [] = fixedWidth 0
    iter (d:ds) = seqWidth (getWidth d) (iter ds)
    
ppStack :: [Doc] -> Doc
ppStack xs = 
  let w = foldr (stackWidth . getWidth) (fixedWidth 0) xs
  in Doc (DStack xs) w

ppLayout :: Int -> Doc -> Doc
ppLayout l d@(Doc _ w) = Doc (DLayout l d) $ fixedWidth l

ppColor :: Color -> Doc -> Doc
ppColor c d@(Doc _ w) = Doc (DColor c d) $ w

ppBox :: Doc -> Doc
ppBox d@(Doc _ w) = Doc (DBox d) $ extWidth 1 $ backExtWidth 1 w

ppFlex :: Int -> Doc
ppFlex w = Doc (DFlex w) $ addFlexWidth w $ fixedWidth 0

(<+>) :: Doc -> Doc -> Doc
(<+>) l r = ppSeq [l, r]

(/+/) :: Doc -> Doc -> Doc 
(/+/) l r = ppStack [l, r]

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
  iter (LFill ch n) acc = iter' n ch acc where
    iter' n ch acc
        | n <= 0    = acc
        | otherwise = iter' (n - 1) ch (ch ++ acc)
  iter LEmpty acc = acc

reduceLines :: [Line] -> Line
reduceLines [] = LEmpty
reduceLines (l:ls) = LConcat l (LConcat (LChar '\n') $ reduceLines ls) 

data Coltrol = CPush Color | CPop

genPString :: Line -> [Either Coltrol Char]
genPString = iter [] where
  iter acc = \case
    LString str -> map Right str ++ acc
    LChar ch    -> Right ch : acc
    LConcat s1 s2 -> iter (iter acc s2) s1
    LFill ch n  -> concat (replicate n (map Right ch)) ++ acc 
    LEmpty      -> acc
    LColor c    -> Left (CPush c) : acc
    LColorPop   -> Left CPop      : acc


resolveColors :: [Either Coltrol Char] -> PString 
resolveColors xs = fst $ runState (iter xs) [] where
  iter :: [Either Coltrol Char] -> State [Color] PString
  iter []                 = return []
  iter ((Right x) : xs)   = (Right x:) <$> iter xs
  iter (Left (CPush c) : xs) = pushCol c >> (((Left $ ansiColorFg c):) <$> iter xs)
  iter (Left CPop : xs)    = do col <- popCol
                                ((Left col) :) <$> iter xs


linesFill :: Line -> Line -> [Line] -> [Line] -> [Line]
linesFill sl sr (l:ls) (r:rs) = LConcat l r : linesFill sl sr ls rs
linesFill sl sr [] (r:rs) = LConcat sl r : linesFill sl sr [] rs
linesFill sl sr (l:ls) [] = LConcat l sr : linesFill sl sr ls []
linesFill _ _ [] [] = []

alignDoc :: Int -> CtxBox -> CtxBox 
alignDoc target c@(xs, fill, current) 
  | target == current = c
  | otherwise = (, LConcat (eLine (target - current)) fill, target) $ do
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
sumFst [x] = [x]

-- 3 state logic --    curr spread, spreads, shifs
type ShiftState = State ((Int, Int), [Scale], [Int])

getShifts :: ShiftState [Int]
getShifts = return . thdOf3 =<< get

putShifts :: [Int] -> ShiftState ()
putShifts = modify . third3 . const

getSpreads :: ShiftState [Scale]
getSpreads = return . sndOf3 =<< get

putSpreads :: [Scale] -> ShiftState ()
putSpreads = modify . snd3 . const

getSpread :: ShiftState (Int, Int)
getSpread = return . fstOf3 =<< get

putSpread :: (Int, Int) -> ShiftState ()
putSpread = modify . fst3 . const

pop :: ShiftState Int
pop = do xs <- getShifts
         putShifts $ sumFst xs
         return (head xs)

popN :: Int -> ShiftState ()
popN n = forM_ [1..n] $ const pop

popSpread :: Int -> Int -> ShiftState ()
popSpread nextShift size = do 
  spreads <- getSpreads
  case spreads of
    [] -> do 
      putSpread (0, 0)
    [x] -> do
      putSpread $ populateSpreadState (size) x
    x : xs -> do
      putSpreads xs
      putSpread $ populateSpreadState nextShift x

shiftBt :: ShiftState a -> ShiftState a
shiftBt m = do st <- get
               res <- m
               put st
               return res

updateSpreads :: Width -> ShiftState ()
updateSpreads = putSpreads . scales

toLines :: Int -> Int -> Doc -> ShiftState CtxBox
toLines size offset (Doc d w) = do
  let totWidth = minWidth $ width w
  case d of
    DEmpty ->      return ([LEmpty], LEmpty, 0)
    DString str -> return ([LString str], eLine totWidth, totWidth)
    DAlignS doc -> do
      shift <- pop
      nextShift <- shiftBt $ pop
      --putSpread $ populateSpreadState (nextShift - shift) (singleScale $ getWidth doc)
      popSpread (nextShift - shift - 1) size
      let missingShift = shift - offset
      (tl, fill, ts) <- toLines (size - missingShift) (offset + missingShift) doc
      let ls = linesFill (eLine missingShift) fill [] tl
      st <- get
      -- (unsafePerformIO $ print st) `seq`
      return (ls, LConcat (eLine missingShift) fill, (missingShift + ts))
    DAlignR doc -> do
      let shifts = shiftAllocation Float size $ shiftList $ getWidth doc
      shiftBt $ do
        updateSpreads $ getWidth $ doc
        putSpread $ populateSpreadState (head shifts - offset) (singleScale $ getWidth doc)
        putShifts shifts
        --st <- get
        --(unsafePerformIO $ print st) `seq`
        toLines (sum shifts) 0 doc
    DSeq ds -> do
      iter size offset ds where
        iter :: Int -> Int -> [Doc] -> ShiftState CtxBox
        iter size off [] = return ([], LEmpty, 0)
        iter size off (d@(Doc _ w):ds) = do
          (os, sep, o) <- toLines size off d 
          (ts, sep', size') <- iter (size - o) (off + o) ds
          let lines = linesFill sep sep' os ts
          return (lines, LConcat sep sep', (o + size')) 
    DStack ds -> do
      spread <- getSpread
      let (size', spread') = assignSpreadState (getFlex $ singleScale w) spread
      putSpread spread'
      ds' <- flip mapM ds $ \doc -> shiftBt $ do 
        updateSpreads $ getWidth $ doc
        nextShift <- shiftBt $ pop
        -- tu jest taki problem, że trzeba zrobić osobnego case'a jeśli nie ma shiftów pod spodem
        -- poza tym jest ok
        popSpread (nextShift - offset) (size' + (getFixed $ singleScale w))
        st <- get
        --(unsafePerformIO $ print ("stack", offset, nextShift, size, size', st)) `seq`
        toLines size offset doc
      let maxSize = foldr (\(_, _, s) s' -> max s s') 0 ds'
      let aligned = map (\box -> alignDoc maxSize box) ds'
      return (concat (map fstOf3 aligned), eLine maxSize, maxSize)
    DColor col doc -> do
      (lines, fill, w) <- toLines size offset doc
      return (map (\l -> LConcat (LColor col) (LConcat l LColorPop)) lines, fill, w)
    DBox doc -> do
      (lines, _, w) <- toLines (size - 2) (offset + 1) doc
      let mapped = map (\l -> LConcat (LString "│") (LConcat l (LString "│"))) lines
          topLine    = (LConcat (LString "╭") (LConcat (LFill "─" w) (LString "╮")))
          bottomLine = (LConcat (LString "╰") (LConcat (LFill "─" w) (LString "╯")))
      return ([topLine]  ++ mapped ++ [bottomLine], eLine $ w + 2, w + 2)
    DEither doc1 doc2 -> undefined
    DLayout size' doc -> toLines size' offset doc
    DCustom d cont -> return $ cont size offset d
    DFlex flex -> do
      spread <- getSpread
      let (size, spread') = assignSpreadState flex spread
      putSpread spread'
      return $ ([eLine size], eLine size, size)

eLine :: Int -> Line
eLine = LFill " "

showDoc :: Doc -> String
showDoc d = do
  let w = minWidth $ width $ getWidth d
  let (ls, _, _) = fst $ runState (toLines w 0 d) ((0, 0), [], [])
  foldAll $ smushAll $ map (resolveColors . genPString) ls

inspectDoc :: Doc -> IO ()
inspectDoc = putStr . showDoc . ppAlignR . makeDoc where
  makeDoc :: Doc -> Doc  
  makeDoc (Doc d w) =
    makeDTree (\x -> ppSeq [x, showWidth w]) d

  showWidth :: Width -> Doc
  showWidth (Fixed x) = ppAlignS $ ppString ("  Fixed " ++ show x)
  showWidth (Shift x y tl) = 
    ppSeq [ ppAlignS $ ppString ("  Shift " ++ show x ++ " " ++ show y)
          , showWidth tl
          ]

  ident :: Doc -> Doc
  ident d = ppSeq [ppString "  ", d]

  makeDTree :: (Doc -> Doc) -> DocTree -> Doc
  makeDTree i (DEmpty) = i $ ppString "Empty"
  makeDTree i (DColor c s) = 
    ppStack [ i $ ppString ("Color " ++ show c), ident $ makeDoc s]
  makeDTree i (DBox s) =
    ppStack [ i $ ppString ("Box"), ident $ makeDoc s]
  makeDTree i (DString s) =
    i $ ppString $ "String: " ++ show s
  makeDTree i (DAlignS s) =
    ppStack [ i $ ppString ("Shift"), ident $ makeDoc s]
  makeDTree i (DAlignR s) =
    ppStack [ i $ ppString ("Reset"), ident $ makeDoc s]
  makeDTree i (DSeq xs) =
    ppStack $ [i $ ppString "Seq"] ++ map (ident . makeDoc) xs
  makeDTree i (DStack xs) =
    ppStack $ [i $ ppString "Stack"] ++ map (ident . makeDoc) xs
  makeDTree i (DFlex n) =
    i $ ppString ("Flex " ++ show n)
  makeDTree i (DLayout n d) =
    ppStack $ [i $ ppString ("Lay " ++ show n), ident $ makeDoc d]
  makeDTree i (DCustom _ _) =
    i $ ppString "Custom"

testInspect = inspectDoc (ppColor CRed ppEmpty)


