{-# LANGUAGE MultiParamTypeClasses 
           , TupleSections 
           , FlexibleInstances
           , FlexibleContexts
           #-}

module TextAlign where

import Pretty
import Pretty2
import Types

data Align
  = ACenter
  | AJustify
  | ALeft
  | ARight

newtype TextDoc = TextDoc { getTextDoc :: String }

instance DocConfCustom TextDoc Align where
  confDocLines align size _ =
    (,0) . formatText align size . getTextDoc

instance Fmt TextDoc where
  format = fmtLift format' where
    format' ""        = Just $ ppConfCustom AJustify
    format' "right"   = Just $ ppConfCustom ARight
    format' "left"    = Just $ ppConfCustom ALeft
    format' "center"  = Just $ ppConfCustom ACenter 
    format' "justify" = Just $ ppConfCustom AJustify
    format' _         = Nothing

ppText :: Align -> String -> Doc
ppText align text = ppConfCustom align (TextDoc text)

paragraphs :: String -> [String]
paragraphs ('\n':'\n':tl) = [] : paragraphs (dropWhile (=='\n') tl)
paragraphs (c:tl) = let r = paragraphs tl
                    in (c:head r) : tail r
paragraphs [] = [[]]

sampleText = "Lorem Ipsum Dolor Sit Amet\n\nWlazl kotek\nna\nplotek i mruga"


takeLen :: [String] -> Int -> ([String], [String])
takeLen ws len = iter ws (len + 1) where
  iter [] _ = ([], [])
  iter (w:ws) maxLen
    | length w + 1 < maxLen = 
      let (l, t) = iter ws (maxLen - 1 - length w)
      in (w:l, t)
    | otherwise =
      ([], w:ws)

test w l = flip takeLen l $ head $ map words $ paragraphs w

{-
decidePenalty :: [String] -> AlignConfig -> ([String], [String])
decidePenalty xs cfg =
  | not $ elem (last xs) (bannedTrailings cfg) = (xs, [])
  | otherwise =
    let rev = revese xs
        len = sum $ map length xs + length xs - 1
-}

injectSpacesRight :: [String] -> Int -> Line
injectSpacesRight ws len =
  let zipped = foldl (\l r -> LConcat l (LConcat (LChar ' ') (LString r))) 
                     (LString $ head ws) 
                     (tail ws)
      totLen = (sum $ map length ws) + length ws - 1
      missing = len - totLen
  in if missing > 0 then
    LConcat zipped (LFill ' ' (Just missing))
  else
    zipped

injectSpacesLeft :: [String] -> Int -> Line
-- injectSpacesLeft [] _ = LEmpty
injectSpacesLeft ws len =
  let zipped = foldl (\l r -> LConcat l (LConcat (LChar ' ') (LString r))) 
                     (LString $ head ws) 
                     (tail ws)
      totLen = (sum $ map length ws) + length ws - 1
      missing = len - totLen
  in if missing > 0 then
    LConcat (LFill ' ' (Just missing)) zipped
  else
    zipped

injectSpacesBoth :: [String] -> Int -> Line
injectSpacesBoth ws len =
  let zipped = foldl (\l r -> LConcat l (LConcat (LChar ' ') (LString r))) 
                     (LString $ head ws) 
                     (tail ws)
      totLen = (sum $ map length ws) + length ws - 1
      missing = len - totLen
  in if missing > 0 then
    let l = missing `div` 2
        r = missing - l
    in LConcat (LFill ' ' (Just l)) $ LConcat zipped (LFill ' ' (Just r))
  else
    zipped

injectSpacesJust :: [String] -> Int -> Line
injectSpacesJust ws len =
  let holes = length ws - 1
      totLen = sum $ map length ws
      missing = len - totLen
      avgGap = missing `div` holes
      overflow = missing - (avgGap * holes)

      zipLines :: [String] -> Int -> Line
      zipLines [ws] _ = LString ws
      zipLines (w:ws) overflow = 
        LConcat (LString w)
                (LConcat (LFill ' ' (Just $ (avgGap + if overflow > 0 then 1 else 0)))
                         (zipLines ws (overflow - 1)))

  in zipLines ws overflow 

alignLine :: Align -> [String] -> Int -> Line
alignLine ALeft = injectSpacesRight
alignLine ARight = injectSpacesLeft
alignLine ACenter = injectSpacesBoth 
alignLine AJustify = injectSpacesJust

formatText :: Align -> Int -> String -> [Line]
formatText align l t = 
  let split = map words $ paragraphs t
      
      consume :: [[String]] -> [Line]
      consume [] = []
      consume ([]:tl) = LFill ' ' (Just l) : consume tl
      consume (ws:tl) =
        let (f, r) = takeLen ws l
            line = alignLine align f l
        in line : consume (r:tl)

  in consume split

run :: Align -> Int -> IO ()
run align width = do
  text <- readFile "Lorem.txt"
  let fmt = genLine $ reduceLines $ formatText align width text
  putStr fmt

superFormat3 =
  runFmt @[(Int, TextDoc)]
         "lay<20><&<s<<d><#<% = <left>>>>>>"
         [ (500, TextDoc $ "Ala ma kota")
         , (5, TextDoc $ "Lorem Ipsum Dolor Sit Amet")
         ]

superFormat4 =
  runFmt @(Int, TextDoc)
         "lay<10><<d><% = <#<justify>>>>"
         (10, TextDoc $ "Ala ma kota")

