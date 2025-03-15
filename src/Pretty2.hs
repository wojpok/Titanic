module Pretty2 where

import Types

widthB :: Doc -> WB
widthB (_, w, _, _) = w

shDepth :: Doc -> ShiftDepth
shDepth (_, _, d, _) = d

widthA :: Doc -> WA
widthA (_, _, _, w) = w

ppEmpty :: Doc
ppEmpty = (DEmpty, 0, Nothing, 0)

ppString :: String -> Doc
ppString str = (DString str, length str, Nothing, 0)

ppShow :: Show a => a -> Doc
ppShow = ppString . show

ppEither :: Doc -> Doc -> Doc
ppEither d1@(t1, wb1, s1, wa1) d2@(t2, wb2, s2, wa2) 
    | wa1 + wb1 < wa2 + wb2     = (DEither d1 d2, wb1, s1, wa1)
    | otherwise                 = (DEither d1 d2, wb2, s2, wa2)

ppAlignR :: Doc -> Doc
ppAlignR d@(_, wb, _, wa) = (DAlignR d, wb + wa, Just wb, 0)

ppAlignS :: Doc -> Doc 
ppAlignS d@(_, wb, _, wa) = (DAlignS d, 0, Just 0, wb + wa)

ppSeq :: [Doc] -> Doc
ppSeq xs = do
    let mapped = iter xs where
        iter :: [Doc] -> ([Doc], WB, ShiftDepth, WA)
        iter [] = ([], 0, Nothing, 0)
        iter [a@(_, wb, s, wa)] = ([a], wb, s, wa)
        iter (d@(_, wb, s, wa):ds) = do
            {-let i = iter ds
            let mapped = case (s, i) of
                    (_, []) -> undefined
                    (Just n, (_, wb', _, wa'):_)        -> (d, wb, s, wa + wb' + wa') 
                    (Nothing, (_, wb', Just n', wa'):_) -> (d, wb + wa + wb', Just (n' + wb + wa), wa')
                    (Nothing, (_, wb', Nothing, wa'):_) -> (d, wb' + wb, Nothing, wa' + wa)
            mapped:i-}
            let i = iter ds
            case (s, i) of
                    (_, ([], _, _, _)) -> undefined
                    (Just n,  (ds, wb', _, wa'))       -> (d:ds, wb, s, wa + wb' + wa') 
                    (Nothing, (ds, wb', Just n', wa')) -> (d:ds, wb + wa + wb', Just (n' + wb + wa), wa')
                    (Nothing, (ds, wb', Nothing, wa')) -> (d:ds, wb' + wb, Nothing, wa' + wa)

    case mapped of 
        ([], _, _, _)   -> ppEmpty
        (ds, wb, s, wa) -> (DSeq ds, wb, s, wa)
    
ppStack :: [Doc] -> Doc
ppStack xs = do
    let maxb = foldl max 0 $ map widthB xs
    let maxa = foldl max 0 $ map widthA xs
    let maxs = foldl max (-1) $ maybeToList $ map shDepth xs

    if maxs == (-1) then
        (DStack xs, maxb, Nothing, maxa)
    else
        (DStack xs, maxb, Just maxs, maxa)

ppLayout :: Int -> Doc -> Doc
ppLayout l d@(_, wb, s, wa) = (DLayout l d, wb, s, wa) 

(+++) :: Doc -> Doc -> Doc
(+++) l r = ppSeq [l, r]

genLine :: Line -> String
genLine line = iter line [] where
    iter :: Line -> String -> String
    iter (LString str) acc = str ++ acc
    iter (LChar ch) acc = ch:acc
    iter (LConcat s1 s2) acc = iter s1 (iter s2 acc)
    iter (LAlignLeft str) acc = iter str acc
    iter (LFill ch Nothing) acc = acc
    iter (LFill ch (Just n)) acc = iter' n ch acc where
        iter' n ch acc
            | n <= 0    = acc
            | otherwise = iter' (n - 1) ch (ch:acc)
    iter LEmpty acc = acc

reduceLines :: [Line] -> Line
reduceLines [] = LEmpty
reduceLines (l:ls) = LConcat l (LConcat (LChar '\n') $ reduceLines ls) 


maybeToList :: [Maybe a] -> [a]
maybeToList (Just a : xs) = a : maybeToList xs
maybeToList (Nothing : xs) = maybeToList xs
maybeToList [] = []

unMaybe :: Maybe a -> a
unMaybe (Just x) = x

linesFill :: Int -> Int -> [Line] -> [Line] -> [Line]
linesFill sl sr (l:ls) (r:rs) = LConcat l r : linesFill sl sr ls rs
linesFill sl sr [] (r:rs) = LConcat (LFill ' ' $ Just sl) r : linesFill sl sr [] rs
linesFill sl sr (l:ls) [] = LConcat l (LFill ' ' $ Just sr) : linesFill sl sr ls []
linesFill _ _ [] [] = []

maybeToInt :: Maybe Int -> Int
maybeToInt Nothing = 0
maybeToInt (Just x) = x

ret :: [Line] -> Int -> CtxBox
ret = (,)  

toLines :: Int -> Int -> Doc -> CtxBox
toLines size off (d, wb, s, wa) = case d of
    DEmpty ->      ret [LEmpty] 0
    DString str -> ret [LString str] wb
    DAlignS doc ->  do
        let (tl, ts) = toLines (size - off) 0 doc
        let ls = linesFill off ts [] tl
        ret ls (off + ts) 
    DAlignR doc -> toLines size (maybeToInt s) doc
    DSeq    ds -> do
        iter size off ds where
            iter :: Int -> Int -> [Doc] -> CtxBox
            iter size off [] = ret [] 0
            iter size off ((d@(d', wb, s, wa)):ds) = do
                let (os, o) = toLines size off d 
                let (ts, size') = iter (size - wb) (off - wb) ds
                let lines = linesFill o size' os ts
                ret lines (o + size') 
    DStack  ds -> do
        let ds' = map (toLines size off) ds
        let aligned = iter ds' where
            iter :: [CtxBox] -> CtxBox
            iter [] = ret [] 0
            iter ((ls, s) : tl) = do
                let (ts, s') = iter tl
                let size = max s s'
                let ls' = linesFill s (size - s) ls [] ++ linesFill s' (size - s') ts []
                ret ls' size
        aligned
    DEither doc1 doc2 -> undefined
    DLayout size' doc -> toLines size' off doc
    DCustom d cont -> cont size off d

showDoc :: Doc -> String
showDoc d = do
    let (ls, _) = toLines 0 0 d
    genLine $ reduceLines ls

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
