{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
           , TypeApplications
           , MultiParamTypeClasses
           , Rank2Types
           #-}

module Pretty where

import Types
import Numeric (showHex)
import Pretty2
import Depth

type Formatter a = String -> Maybe (a -> Doc)

class Fmt a where
  format :: Formatter a 
  fmtApp :: String -> a -> Maybe Doc
  fmtApp fmt val = do format fmt <*> pure val 

class Fmt2 a where
  format2 :: (forall a. Formatter a -> Formatter a) -> Formatter a 

newtype FmtTrans = FmtTrans 
  { runFmtTrans :: forall a. Formatter a -> Formatter a }

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


fmtLift :: forall a. Formatter a -> Formatter a
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

instance Fmt2 Int where
  format2 _ "" = Just ppShow
  format2 _ "x" = Just $ (ppString . ("0x" ++) . flip showHex "")
  format2 _ _ = Nothing

instance Fmt2 a => Fmt2 [a] where
  format2 lift f = 
    case f of
      "" -> (ppStack .) <$> fmap <$> (lift (format2 lift)) ""
      's':tl -> do tfmt <- brMatch tl
                   tf <- lift (format2 lift) (fst tfmt)
                   Just $ (ppSeq . fmap tf) 
      _ -> Nothing 

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
