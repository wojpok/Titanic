{-# LANGUAGE AllowAmbiguousTypes
           , TypeApplications
           , TypeSynonymInstances
           , FlexibleInstances
           , MultilineStrings
           , Rank2Types
           , ConstrainedClassMethods
           , InstanceSigs
           , ScopedTypeVariables
           #-}

module Pretty3 where

import Parser 
import Types
import Numeric (showHex)
import Pretty2
import Depth
import Colors

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative ((<|>))
import Data.Either

type Err a = Either String a

type Formatter a = CoreStyle -> Err (a -> Doc)

instance MonadFail (Either String) where
  fail = Left

lookupE :: Eq a => a -> [(a, b)] -> Err b
lookupE _ [] = Left ""
lookupE x ((y, v):ys)
  | x == y = return v
  | otherwise = lookupE x ys 

getSStr :: String -> CoreStyle -> Err String
getSStr ident (CoreStyle _ sty _) = do
  v <- lookupE ident (getStyleDict sty)
  case v of
    SString s -> return s
    _ -> Left "Expected string" 

getSStrI :: String -> CoreStyle -> Err String
getSStrI ident n = getSStr "im" n `alter` getSStr ident n

getSCol :: String -> CoreStyle -> Err Color 
getSCol ident (CoreStyle _ sty _) = do
  v <- lookupE ident (getStyleDict sty)
  case v of
    SColor s -> return s
    _ -> Left "Expected string" 

getSColI :: String -> CoreStyle -> Err Color
getSColI ident n = getSCol "im" n `alter` getSCol ident n

alter :: Err a -> Err a -> Err a
alter (Right x) _ = Right x
alter _ r = r

class Formatable a where
  name :: a -> String
  fmt :: Formatter a

instance Formatable () where
  name = const "unit"
  fmt = fmtLift $ \_ -> return $ const ppEmpty

instance Formatable Int where
  name = const "int"
  fmt = fmtLift $ \doc -> do
    t <- getSStrI "repr" doc `alter` pure "n"
    case t of
      "n" -> return (ppShow)
      "x" -> return (ppString . ("0x" ++) . flip showHex "")
      _ -> fail "Unknown representation"

instance Formatable Bool where
  name = const "bool"
  fmt = fmtLift $ \doc -> do
    t <- getSStrI "repr" doc `alter` pure "n"
    case t of
      "n" -> return ppShow
      "lisp" -> return $ \x -> ppShow $ if x then "#t"   else "#f"
      "ml"   -> return $ \x -> ppShow $ if x then "true" else "false"
      "num"  -> return $ \x -> ppShow $ if x then "1"    else "0"

instance Formatable a => Formatable [a] where
  name = const "list"
  fmt = fmtLift $ \doc -> do
    t <- getSStrI "orient" doc `alter` pure "hor"
    r <- assertSingle doc
    fmtRec <- fmt r
    case t of
      "ver" -> do spacing <- getSStr "s" doc `alter` pure ""
                  return (ppSeq . interleave spacing . map fmtRec) where
                    interleave spacing [] = []
                    interleave spacing [x] = [x]
                    interleave spacing (x:xs) = (x : ppString spacing : interleave spacing xs)
      "hor" -> return (ppStack . map fmtRec)

instance (Formatable a, Formatable b) => Formatable (a, b) where
  name = const "tuple2"
  fmt :: (Formatable a, Formatable b) => Formatter (a, b)
  fmt = fmtLift $ \doc -> do
    let (CoreStyle _ _ struct) = doc
        mapped = map recf struct
        recf :: CoreInterp -> Err ((a, b) -> Doc) 
        recf (CoreInterp 1 f) = fmt @a  f >>= \fx -> pure (fx . fst)
        recf (CoreInterp 2 f) = fmt @b  f >>= \fx -> pure (fx . snd)
        recf (CoreInterp _ f) = fmt @() f >>= \fx -> pure (fx . const ())
    fs <- sequence mapped
    return $ \p -> (ppSeq $ fs <*> [p])


pipe :: Formatable a => String -> a -> Err Doc
pipe style d = do
  sty <- l $ parseStyle style 
  f <- fmt $ desugar sty
  return $ f d
    where
      l (Left _) = Left "Parse error"
      l (Right x) = Right x

makeFmt :: Formatable a => String -> a -> IO ()
makeFmt style d = do
  case pipe style d of
    Left e -> putStrLn e
    Right d -> putStr $ showDoc d

testNewFormat = 
  makeFmt @[[Int]]
          """
          (color:red
            *(list:hor
              (list {s: ' ', orient: ver} 
                &(box 
                  (color:cyan 
                    (int {repr: 'x'})
                  )
                )
              )
            )
          )
          """
          [[10, 20, 30], [5000, 50000, 6000]]

testNewFormat2 = 
  makeFmt @[(Int, Int)]
          """
          (color:green
            (box
              (color:white
                *(list:hor
                  (tuple2 
                    % %
                    $1(int:x) 
                    &(color:red % = %) 
                    $1(int) 
                    &(color:cyan % ; %) 
                    $2(int)
                    % %)))))
          """
          [(1, 22334), (3324324, 4), (5, 6)]

assertSingle :: CoreStyle -> Err CoreStyle
assertSingle (CoreStyle _ _ [CoreInterp _ x]) = Right x
assertSingle _ = Left "Arrity mismatch"

fmtLift :: forall a. Formatter a -> Formatter a
fmtLift cont c@(CoreStyle name sty struct) =
  case name of
    "box" -> do tl <- assertSingle c
                fmt <- fmtLift cont tl
                return (ppBox . fmt)
    "color" -> do tl <- assertSingle c
                  col <- getSColI "col" c
                  fmt <- fmtLift cont tl
                  return (ppColor col . fmt)
    "shift" -> do tl <- assertSingle c
                  (ppAlignS .) <$> fmtLift cont tl
    "reset" -> do tl <- assertSingle c
                  (ppAlignR .) <$> fmtLift cont tl
    "string" -> do text <- getSStrI "str" c
                   return $ const $ ppString text
    _ -> cont c
