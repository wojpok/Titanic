{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
           , TypeApplications
           #-}

module Pretty where

import Numeric (showHex)
import Pretty2

class Fmt a where
  format :: String -> Maybe (a -> Doc)  
  fmtApp :: String -> a -> Maybe Doc
  fmtApp fmt val = do f <- format fmt
                      return $ f val 

instance {-# OVERLAPPABLE #-} (Show a) => Fmt a where
  format _ = Just ppShow

instance Fmt Int where
  format "" = Just ppShow
  format "d" = Just ppShow
  format "x" = Just $ (ppString . ("0x" ++) . flip showHex "")
  format _ = Nothing

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

instance Fmt a => Fmt [a] where
  format "" = do tfmt <- format ""
                 Just $ (ppStack . fmap tfmt)
  format ('s':tl) = do tfmt <- brMatch tl
                       tf <- format (fst tfmt)
                       Just $ (ppStack . fmap tf)
  format ('r':tl) = do tfmt <- brMatch tl
                       tf <- format (fst tfmt)
                       Just $ (ppSeq . fmap tf)
  format _ = Nothing


