{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
           , TypeApplications
           , MultiParamTypeClasses
           #-}

module Pretty where

import Types
import Numeric (showHex)
import Pretty2

class Fmt a where
  format :: String -> Maybe (a -> Doc)  
  fmtApp :: String -> a -> Maybe Doc
  fmtApp fmt val = do f <- format fmt
                      return $ f val 

runFmt :: Fmt a => String -> a -> IO ()
runFmt fmt d = 
  case fmtApp fmt d of
    Just doc -> putStr $ showDoc doc
    Nothing  -> putStrLn "Format error"

brMatch :: String -> Maybe (String, String)
brMatch [] = Just ("", "")
brMatch (c:tl)
  | c /= '<' = Nothing
  | otherwise = let iter :: String -> String -> Int -> Maybe (String, String)
                    iter ('>':tl) acc 0 = Just (reverse acc, tl)
                    iter [] _ _ = Nothing
                    iter ('<':tl) acc l = iter tl ('<':acc) (l + 1)
                    iter ('>':tl) acc l = iter tl ('>':acc) (l - 1)
                    iter (c:tl) acc l = iter tl (c:acc) l
                in iter tl [] 0

formatList :: String -> Maybe [String]
formatList [] = Just [""]
formatList fmt = 
  let prefix = takeWhile (/='<') fmt
      suffix = dropWhile (/='<') fmt

      -- monomorphic folding under pair, might be done as one-liner
      fs :: String -> Maybe [String]
      fs [] = Just []
      fs w = do (f1, tl) <- brMatch w
                (f1:) <$> fs tl
  in do fmts <- fs suffix
        return (prefix:fmts)

comb :: (a -> a) -> Maybe (c -> a) -> Maybe (c -> a)
comb f m = do
  fm <- m
  return $ f . fm

fmtLift :: (String -> Maybe (a -> Doc)) -> String -> Maybe (a -> Doc)
fmtLift cont "" = cont ""
fmtLift cont fmt = 
  case formatList fmt of
    Just ["lay", num, r] -> comb (ppLayout (read num)) $ fmtLift cont r
    Just ["&", r] -> comb ppAlignR $ fmtLift cont r
    Just ["#", r] -> comb ppAlignS $ fmtLift cont r
    Just [('%':str), r] -> comb (\d -> ppSeq [(ppString str), d]) $ fmtLift cont r
    Just [('%':str)] -> Just (const $ ppString str)
    Just _ -> cont fmt
    Nothing -> Nothing

instance {-# OVERLAPPABLE #-} (Show a) => Fmt a where
  format = fmtLift $ const $ Just ppShow

instance Fmt Int where
  format = fmtLift format' where
    format' "" = Just ppShow
    format' "d" = Just ppShow
    format' "x" = Just $ (ppString . ("0x" ++) . flip showHex "")
    format' _ = Nothing

instance Fmt a => Fmt [a] where
  format = fmtLift format' where
    format' "" = do tfmt <- format ""
                    Just $ (ppStack . fmap tfmt)
    format' ('s':tl) = do tfmt <- brMatch tl
                          tf <- format (fst tfmt)
                          Just $ (ppStack . fmap tf)
    format' ('r':tl) = do tfmt <- brMatch tl
                          tf <- format (fst tfmt)
                          Just $ (ppSeq . fmap tf)
    format' _ = Nothing

instance (Fmt a, Fmt b) => Fmt (a, b) where
  format = fmtLift format' where
    format' "" = do rfmt <- format ""
                    lfmt <- format ""
                    Just (\(l, r) -> ppSeq [lfmt l, rfmt r])
    format' fmt =
      case formatList fmt of
      Just ["", lf, rf] -> do lfmt <- format lf
                              rfmt <- format rf
                              Just (\(l, r) -> ppSeq [lfmt l, rfmt r])
      _ -> Nothing

superFormat = 
  runFmt @[(Int, Int)]
         "lay<100><&<s<<d><#<% = <d>>>>>>"
         [ (10, 10000000)
         , (2000, 2)
         , (3, 30000)
         , (4000000, 4)
         ]

superFormat2 = runFmt @(Int, Int) "&<d><#d>" (10, 200)
